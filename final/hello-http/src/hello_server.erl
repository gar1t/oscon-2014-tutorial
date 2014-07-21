-module(hello_server).

-behavior(e2_task).

-export([start_link/0, init/1, handle_task/1]).

-define(PORT, 8080).

%%===================================================================
%% Start / init
%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, ?PORT).

init(Port) ->
    {ok, listen(Port)}.

listen(Port) ->
    Opts =
	[{reuseaddr, true},
	 {active, false},
	 {packet, http},
	 {backlog, 1024}],
    {ok, LSock} = gen_tcp: listen(Port, Opts),
    io:format("Listing on port ~b~n", [Port]),
    LSock.

%%===================================================================
%% Handle client connections
%%===================================================================

handle_task(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    handle_request(recv_request(Sock), Sock),
    {repeat, LSock}.

%%===================================================================
%% Request support
%%===================================================================

recv_request(Sock) ->
    recv_request(Sock, init_request()).

init_request() ->
    #{ headers => [] }.

recv_request(Sock, Req) ->
    handle_request_packet(recv_packet(Sock), Req, Sock).

recv_packet(Sock) ->
    gen_tcp:recv(Sock, 0).

handle_request_packet({ok, {http_request, Method, {abs_path, Path}, Ver}},
		      Req, Sock) ->
    recv_request(Sock, add_request_line(Req, Method, Path, Ver));
handle_request_packet({ok, {http_header, _, Name ,_ , Value}},
		      Req, Sock) ->
    recv_request(Sock, add_request_header(Req, Name, Value));
handle_request_packet({ok, http_eoh}, Req, _Sock) ->
    {ok, Req};
handle_request_packet({ok, {http_error, Err}}, _Req, _Sock) ->
    {error, Err};
handle_request_packet({error, Err}, _Req, _Sock) ->
    {error, Err}.

add_request_line(Req, Method, Path, Ver) ->
    Req#{method       => Method,
	 abs_path     => Path,
	 http_version => Ver}.

add_request_header(#{ headers := Headers}=Req, Name, Value) ->
    Req#{ headers => [{Name, Value}|Headers] }.

handle_request({ok, Req}, Sock) ->
    Resp = response(Req),
    send_response(Sock, Resp),
    ok = gen_tcp:close(Sock);
handle_request({error, closed}, _Sock) ->
    ok.

%%===================================================================
%% Response support
%%===================================================================

response(#{ method := Method, abs_path := Path }) ->
    Status = {200, "OK"},
    Body = io_lib:format("You want to ~s ~s right?\n", [Method, Path]),
    BodyLen = iolist_size(Body),
    Headers =
	[{"Content-Type", "text/plain"},
	 {"Content-Length", integer_to_list(BodyLen)}],
    {Status, Headers, Body}.

send_response(Sock, {Status, Headers, Body}) ->
    Resp = [format_response_line(Status),
	    format_response_headers(Headers),
	    Body],
    ok = gen_tcp:send(Sock, Resp).

format_response_line({Code, Desc}) ->
    ["HTTP/1.0 ", integer_to_list(Code), " ", Desc, "\r\n"].

format_response_headers(Headers) ->
    [[header_line(Name, Value)
      || {Name, Value} <- Headers],
     "\r\n"].

header_line(Name, Value) ->
    [Name, ": ", Value, "\r\n"].

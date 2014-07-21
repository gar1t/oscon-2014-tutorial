-module(http_util).

-export([listen/1,
	 recv_request/1,
	 send_response/2,
	 send_response_and_close/2]).

%%===================================================================
%% listen
%%===================================================================

listen(Port) ->
    Opts =
	[binary,
	 {active, false},
	 {packet, http},
	 {backlog, 1024},
	 {nodelay, true},
	 {reuseaddr, true}],
    {ok, LSock} = gen_tcp: listen(Port, Opts),
    io:format("Listing on port ~b~n", [Port]),
    LSock.

recv_request(Sock) ->
    recv_request(Sock, init_request()).

init_request() ->
    #{ headers => [] }.

%%===================================================================
%% recv_request
%%===================================================================

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

%%===================================================================
%% send_response
%%===================================================================

send_response_and_close(Sock, Resp) ->
    send_response(Sock, Resp),
    try_close(Sock).

send_response(Sock, {Status, Headers, Body}) ->
    Resp = 
	[format_response_line(Status),
	 format_response_headers(
	   ensure_content_length(Headers, Body)),
	 Body],
    gen_tcp:send(Sock, Resp).

format_response_line({Code, Status}) ->
    ["HTTP/1.0 ", integer_to_list(Code), " ", Status, "\r\n"].

ensure_content_length(Headers, Body) ->
    maybe_add_content_length(
      not lists:keymember("Content-Length", 1, Headers),
      Headers, Body).

maybe_add_content_length(true, Headers, Body) ->
    Len = iolist_size(Body),
    [{"Content-Length", integer_to_list(Len)}|Headers];
maybe_add_content_length(false, Headers, _Body) ->
    Headers.

format_response_headers(Headers) ->
    [[header_line(Name, Value)
      || {Name, Value} <- Headers],
     "\r\n"].

header_line(Name, Value) ->
    [Name, ": ", Value, "\r\n"].

try_close(Sock) ->
    case gen_tcp:close(Sock) of
	ok -> ok;
	{error, closed} -> ok;
	{error, Err} -> error({close, Err})
    end.

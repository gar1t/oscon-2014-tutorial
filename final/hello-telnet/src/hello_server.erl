-module(hello_server).

-behavior(e2_task).

-export([start_link/0, init/1, handle_task/1]).

-define(PORT, 5000).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, ?PORT).

init(Port) ->
    {ok, listen(Port)}.

listen(Port) ->
    Opts = [{reuseaddr, true}],
    {ok, LSock} = gen_tcp: listen(Port, Opts),
    io:format("Listing on port ~b~n", [Port]),
    LSock.

%%%===================================================================
%%% Handle client connections
%%%===================================================================

handle_task(LSock) ->
    handle_client(accept(LSock)),
    {repeat, LSock}.

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    ok = inet:setopts(Sock, [{active, false}, {packet, line}]),
    io:format("Handling client connection~n"),
    Sock.

handle_client(Sock) ->
    handle_line(recv_line(Sock), Sock).

recv_line(Sock) ->
    gen_tcp:recv(Sock, 0).

handle_line({ok, "quit\r\n"}, Sock) ->
    ok = gen_tcp:close(Sock);
handle_line({ok, Line}, Sock) ->
    reply(Sock, Line);
handle_line({error, closed}, _Sock) ->
    io:format("Connection closed by client~n").

reply(Sock, Line) ->
    ok = gen_tcp:send(Sock, ["You said, ", Line]),
    handle_client(Sock).

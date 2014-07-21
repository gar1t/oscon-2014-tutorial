-module(hello_handler).

-behavior(e2_task).

-export([start_link/1, handle_task/1]).

start_link(Sock) ->
    e2_task:start_link(?MODULE, Sock).

handle_task(Sock) ->
    handle_request(http_util:recv_request(Sock), Sock),
    {stop, normal}.

handle_request({ok, Req}, Sock) ->
    http_util:send_response_and_close(Sock, response(Req));
handle_request({error, closed}, _Sock) ->
    ok.

response(#{ method := Method, abs_path := Path }) ->
    Status = {200, "OK"},
    Headers = [{"Content-Type", "text/plain"}],
    Body = io_lib:format("You want to ~s ~s right?\n", [Method, Path]),
    {Status, Headers, Body}.

-module(hello_task).

-behavior(e2_task).

-export([start_link/0, handle_task/1]).

start_link() ->
    e2_task:start_link(?MODULE, 1).

handle_task(N) ->
    io:format("Hello #~b!~n", [N]),
    {repeat, N, 1000}.

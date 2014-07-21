-module(hello_handler_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_handler/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hello_handler, [registered]).

start_handler(Sock) ->
    e2_task_supervisor:start_task(?MODULE, [Sock]).

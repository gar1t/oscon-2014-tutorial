-module(hello).

-export([start/0, stop/0, restart/0]).

start() ->
    e2_application:start_with_dependencies(hello).

stop() ->
    application:stop(hello).

restart() ->
    stop(),
    start().

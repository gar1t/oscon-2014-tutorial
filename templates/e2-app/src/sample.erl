-module(sample).

-export([start/0, stop/0, restart/0]).

start() ->
    e2_application:start_with_dependencies(sample).

stop() ->
    application:stop(sample).

restart() ->
    stop(),
    start().

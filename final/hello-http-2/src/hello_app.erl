-module(hello_app).

-export([init/0]).

init() ->
    {ok, [hello_handler_sup, hello_server]}.

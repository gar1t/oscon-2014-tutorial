-module(hello_server).

-behavior(e2_task).

-export([start_link/0, init/1, handle_task/1]).

-define(PORT, 8080).

start_link() ->
    e2_task:start_link(?MODULE, ?PORT, [registered]).

init(Port) ->
    LSock = http_util:listen(Port),
    {ok, #{ lsock => LSock}}.

handle_task(#{ lsock := LSock }=State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, _} = hello_handler_sup:start_handler(Sock),
    {repeat, State}.

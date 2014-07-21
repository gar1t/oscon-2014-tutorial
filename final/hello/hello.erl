-module(hello).

-export([say_once/0, say_repeating/0]).

say() ->
    io:format("Hello!~n").

say_repeating() -> say(1).

say(N) ->
    io:format("Hello #~b!~n", [N]),
    timer:sleep(1000),
    say(N + 1).

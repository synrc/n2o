-module (default_security_handler).
-author('Rusty Klophaus').
-behaviour (security_handler).
-export ([init/2, finish/2]).

init(_Config, State) -> {ok, State}.

finish(_Config, State) -> {ok, State}.

-module(default_identity_handler).
-author('Rusty Klophaus').
-behaviour (identity_handler).
-export([init/2, finish/2, get_user/0, set_user/1, clear/0]).
-define(KEY, <<"user">>).

init(_Config, State) -> {ok, State}.
finish(_Config, State) -> {ok, State}.
get_user() -> wf:session(?KEY).
set_user(User) -> wf:session(?KEY, User).
clear() -> wf:session(?KEY, undefined).

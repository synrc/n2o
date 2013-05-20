-module(default_role_handler).
-author('Rusty Klophaus').
-behaviour(role_handler).
-export([init/2, finish/2, get_has_role/1, set_has_role/2, get_roles/0, clear_all/0]).
-define(KEY, <<"roles">>).

init(_Config, State) -> {ok, State}.
finish(_Config, State) -> {ok, State}.

get_has_role(Role) -> 
    Roles = wf:session_default(?KEY, []),
    lists:member(Role, Roles).

set_has_role(Role, IsInRole) -> 
    Roles = wf:session_default(?KEY, []),
    Roles1 = Roles -- [Role],
    case IsInRole of
        true -> wf:session(?KEY, [Role|Roles1]);
        _    -> wf:session(?KEY, Roles1)
    end.

get_roles() -> wf:session_default(?KEY, []).
clear_all() -> wf:session(?KEY, []).

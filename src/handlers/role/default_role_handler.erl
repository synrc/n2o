% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_role_handler).
-behaviour (role_handler).
-export ([
    init/2, 
    finish/2,
    get_has_role/3, 
    set_has_role/4, 
    get_roles/2,
    clear_all/2
]).
-define(KEY, {default_role_handler, roles}).

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

get_has_role(Role, _Config, _State) -> 
    Roles = wf:session_default(?KEY, []),
    lists:member(Role, Roles).

set_has_role(Role, IsInRole, _Config, State) -> 
    Roles = wf:session_default(?KEY, []),
    Roles1 = Roles -- [Role],
    case IsInRole of
        true -> wf:session(?KEY, [Role|Roles1]);
        _    -> wf:session(?KEY, Roles1)
    end,
    {ok, State}.

get_roles(_Config, _State) -> 
    wf:session_default(?KEY, []).

clear_all(_Config, State) -> 
    wf:session(?KEY, []),
    {ok, State}.

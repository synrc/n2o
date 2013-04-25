% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_identity_handler).
-behaviour (identity_handler).
-export ([
    init/2, 
    finish/2,
    get_user/2, 
    set_user/3,
    clear/2
]).
-define(KEY, {default_identity_handler, user}).

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

get_user(_Config, _State) -> 
    wf:session(?KEY).

set_user(User, _Config, State) -> 
    wf:session(?KEY, User),
    {ok, State}.

clear(_Config, State) -> 
    wf:session(?KEY, undefined),
    {ok, State}.

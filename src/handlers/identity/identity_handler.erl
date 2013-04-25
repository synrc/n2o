% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (identity_handler).
-export ([
    behaviour_info/1, get_user/0, set_user/1, clear/0
]).



% get_user(State) -> User.
% Retrieve an Erlang term representing the current user.
get_user() ->
    _User = wf_handler:call_readonly(identity_handler, get_user).

% set_user(User, State) -> {ok, NewState}.
% Set an Erlang term representing the current user.
set_user(User) ->
    ok = wf_handler:call(identity_handler, set_user, [User]).

% clear(State) -> {ok, NewState}.
% Set the user to undefined.
clear() ->
    ok = wf_handler:call(identity_handler, clear).



behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_user, 2},
    {set_user, 3},
    {clear, 2}
];
behaviour_info(_) -> undefined.

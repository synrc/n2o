-module (identity_handler).
-author('Rusty Klophaus').
-export ([ behaviour_info/1, get_user/0, set_user/1, clear/0]).

get_user() -> wf_handler:call_readonly(identity_handler, get_user).
set_user(User) -> wf_handler:call(identity_handler, set_user, [User]).
clear() -> wf_handler:call(identity_handler, clear).

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_user, 2},
    {set_user, 3},
    {clear, 2}
];
behaviour_info(_) -> undefined.

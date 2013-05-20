-module (identity_handler).
-author('Rusty Klophaus').
-export ([ behaviour_info/1, get_user/0, set_user/1, clear/0]).

-define(IDENTITY, default_identity_handler).

get_user() -> ?IDENTITY:get_user().
set_user(User) -> ?IDENTITY:set_user(User).
clear() -> ?IDENTITY:clear().

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_user, 0},
    {set_user, 1},
    {clear, 0}
];
behaviour_info(_) -> undefined.

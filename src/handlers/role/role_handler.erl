-module(role_handler).
-author('Rusty Klophaus').
-export([behaviour_info/1, get_has_role/1, set_has_role/2, get_roles/0, clear_all/0]).

-define(ROLES, default_role_handler).

get_has_role(Role) ->  ?ROLES:get_has_role(Role).
set_has_role(Role, IsInRole) -> ?ROLES:set_has_role(Role, IsInRole).
get_roles() -> ?ROLES:get_roles().
clear_all() -> ?ROLES:clear_all().

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_has_role, 1},
    {set_has_role, 2},
    {get_roles, 0},
    {clear_all, 0}
];
behaviour_info(_) -> undefined.

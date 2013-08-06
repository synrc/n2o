-module(security_handler).
-author('Rusty Klophaus').
-export ([behaviour_info/1]).

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2}
];

behaviour_info(_) -> undefined.

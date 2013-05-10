-module(process_registry_handler).
-author('Rusty Klophaus').
-export([behaviour_info/1, get_pid/1, get_pid/2]).

% get_pid(Key, State) -> {ok, Pid, NewState}.
% Get the process associated with this Key.
get_pid(Key) ->
    case wf_handler:call(process_registry_handler, get_pid, [Key]) of
        {ok, undefined} -> undefined;
        {ok, Pid} -> {ok, Pid}
    end.

% get_pid(Key, Function, State) -> {ok, Pid, NewState}.	
% Return the process associated with Key. If that process does not
% exist, then create a new process and associate it with Key.
get_pid(Key, Function) ->
    {ok, _Pid} = wf_handler:call(process_registry_handler, get_pid, [Key, Function]).

behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {get_pid, 3},
    {get_pid, 4}
];

behaviour_info(_) -> undefined.

-module(wf_handler).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

handle(Handler, Fun) -> handle(Handler, Fun, []).
handle(Handler = #handler_context { name = Name, module=Mod, config=Config, state=State }, Fun, Args) ->
    Result = erlang:apply(Mod,Fun, Args ++ [Config, State]),
    case Result of
        {ok, NewState} -> update_handler(Name, Handler, NewState), ok;
        {ok, Value, NewState} -> update_handler(Name, Handler, NewState), {ok, Value};
        {ok, Value1, Value2, NewState} -> update_handler(Name, Handler, NewState), {ok, Value1, Value2}
    end.

update_handler(Name, Handler, State) ->
    NewHandler = Handler#handler_context { state=State },
    put(Name,Handler).

call(Name, FunctionName) -> call(Name, FunctionName, []).
call(Name, FunctionName, Args) -> Handler  = get_handler(Name), handle(Name, Handler, Args).
call_readonly(Name, FunctionName) -> call_readonly(Name, FunctionName, []).
call_readonly(Name, FunctionName, Args) ->
    #handler_context { module=Module, config=Config, state=State } = get_handler(Name),
    erlang:apply(Module, FunctionName, Args ++ [Config, State]).

set_handler(Name, Module, Config) -> put(Name,#handler_context{name=Name,module=Module,config=Config,state=[]}).
get_handler(Name) -> get(Name).

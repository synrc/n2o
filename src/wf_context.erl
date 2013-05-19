-module(wf_context).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

params() -> get(params).
params(Params) -> put(params,Params).
page_module() -> get(page_module).
page_module(Module) -> put(page_module,Module).
actions() -> get(actions).
actions(Actions) -> put(actions,Actions).
script() -> get(script).
script(Script) -> put(script,Script).
clear_actions() -> put(actions,[]).
add_action(Action) ->
    Actions = case get(actions) of undefined -> []; E -> E end,
    put(actions,Actions ++ [Action]).

make_handler(Name, Module) -> #handler{name=Name, module=Module, state=[]}.
init_context(Req) ->
    #context{
        actions=[],
        module=index,
        path=[],
        req=Req,
        params=[],
        session=undefined,
        handlers= [ make_handler(query_handler,   n2o_query_handler),
                    make_handler(session_handler, n2o_session_handler),
                    make_handler(route_handler,   n2o_route_handler) ]}.

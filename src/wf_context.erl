-module(wf_context).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

context() -> get(context).
context(Ctx) -> put(context,Ctx).
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

init_context(Req) ->
    #context{
        actions=[],
        module=index,
        path=[],
        req=Req,
        params=[],
        session=undefined,
        handlers= [ {'query', handler_coalecse('query', n2o_query_handler)},
                    {session, handler_coalecse(session, n2o_session_handler)},
                    {route,   handler_coalecse(route, n2o_route_handler)} ]}.

handler_coalecse(Key, Default) -> case application:get_env(n2o,Key) of
                              undefined -> Default;
                              {ok,V} -> V end.

-module(wf_context).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

context() -> get(context).
context(Ctx) -> put(context,Ctx).
actions() -> get(actions).
actions(Actions) -> put(actions,Actions).
cookies() -> C = get(cookies), case is_list(C) of true -> C; _ -> [] end.
add_cookie(Name,Value,Path,TTL) -> 
    C = cookies(),
    Cookies = case lists:keyfind(Name,1,C) of
        {Name,_,_,_} -> lists:keyreplace(Name,1,C,{Name,Value,Path,TTL});
        false -> [{Name,Value,Path,TTL}|C] end,
    put(cookies,Cookies).

script() -> get(script).
script(Script) -> put(script,Script).
clear_actions() -> put(actions,[]).
add_action(Action) ->
    Actions = case get(actions) of undefined -> []; E -> E end,
    put(actions,Actions++[Action]).

init_context(Req) ->
    #context{
        actions=[], module=index, path=[], req=Req, params=[],
        handlers= [ {'query', wf:config(n2o,'query', n2o_query)},
                    {session, wf:config(n2o,session, n2o_session)},
                    {route,   wf:config(n2o,route,   n2o_dynroute)} ]}.

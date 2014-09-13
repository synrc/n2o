-module(n2o_session).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export(?SESSION_API).
-compile(export_all).
-record(state, {unique, node}).

init2(State, Req) -> {ok, State, Req}.

init(State, Ctx) -> 
    C = wf:cookie_req(session_cookie_name(),Ctx#cx.req),
    SessionId = case C of
                     undefined -> undefined;
                     _Else -> _Else end,
    {{D1,D2,D3},{T1,T2,T3}} = calendar:now_to_datetime(now()),
    Till = {{D1,D2,D3+1},{T1,T2,T3}},
    TTL = 24 * 60 * 60, % 1 day TTL
    SessionCookie = case lookup_ets({SessionId,<<"auth">>}) of 
                 undefined -> Cookie = {{new_cookie_value(),<<"auth">>},<<"/">>,now(),{TTL,Till},new},
                              ets:insert(cookies,Cookie),
                              wf:info(?MODULE,"Cookie New: ~p",[Cookie]),
                              Cookie;
                 {{Session,Key},Path,Issued,{_TTL,_Till},Status} -> case expired(Issued,{_TTL,_Till}) of
                     false -> Cookie = {{Session,Key},Path,Issued,{_TTL,_Till},Status},
                              wf:info(?MODULE,"Cookie Same: ~p",[Cookie]),
                              Cookie;
                      true -> Cookie = {{new_cookie_value(),<<"auth">>},<<"/">>,now(),{TTL,Till},new},
                              ets:insert(cookies,Cookie), 
                              wf:info(?MODULE,"Cookie Expired: ~p",[Cookie]),
                              Cookie end;
                 _ -> error_logger:info_msg("Cookie Error"), skip
                      end,
    wf:info(?MODULE,"State: ~p",[SessionCookie]),
    {ok, State, Ctx#cx{session=SessionCookie}}.

expired(_Issued,{_TTL,Till}) -> false. %Till < calendar:now_to_datetime(now()).

finish(State, Ctx) -> 
    wf:info(?MODULE,"Finish Cookie Set ~p",[State]),
    NewReq = case Ctx#cx.session of
         {{Session,Key},Path,Issued,{TTL,Till},Status} -> 
              wf:cookie_req(session_cookie_name(),Session,Path,TTL,Ctx#cx.req);
         _ -> Ctx#cx.req end,
    {ok, [], Ctx#cx{req=NewReq}}.

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
    wf:info(?MODULE,"Lookup ETS: ~p",[{Res,Key}]),
    case Res of 
         [] -> undefined;
         [Value] -> Value;
         Values -> Values end.

clear() -> clear(session_id()).
clear(Session) ->
    [ ets:delete(cookies,X) || X <- ets:select(cookies,
        ets:fun2ms(fun(A) when (element(1,element(1,A)) == Session) -> element(1,A) end)) ].

session_id() -> wf:cookie_req(session_cookie_name(),?REQ).
new_cookie_value() -> base64:encode(erlang:md5(term_to_binary({now(), make_ref()}))).
new_state() -> #state{unique=new_cookie_value()}.
session_cookie_name() -> wf:config(n2o, session_cookie_name, <<"n2o-sid">>).
set_value(Key, Value) -> ets:insert(cookies,{{session_id(),Key},Value}), Value.
get_value(Key, DefaultValue) -> 
    Res = case lookup_ets({session_id(),Key}) of
               undefined -> DefaultValue;
               {_,Value} -> Value end,
    wf:info(?MODULE,"Session Lookup Key ~p Value ~p",[Key,Res]),
    Res.

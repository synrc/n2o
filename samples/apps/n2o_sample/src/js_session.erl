-module(js_session).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export(?SESSION_API).
-compile(export_all).
-record(state, {unique, node}).

init(State,Ctx) -> {ok,State,Ctx}.
finish(State,Ctx) -> {ok,State,Ctx}.

ensure_sid(State, Ctx) -> 
    SessionUser = wf:cookie_req(<<"n2o-name">>,?REQ),
    SessionId   = wf:cookie_req(<<"n2o-sid">>, ?REQ),
    wf:info(?MODULE,"Session Init n2o-sid: ~p",[SessionId]),
    {{D1,D2,D3},{T1,T2,T3}} = calendar:now_to_datetime(now()),
    Till = {{D1,D2,D3+1},{T1,T2,T3}},
    TTL = 24 * 60 * 60, % 1 day TTL
    SessionCookie = case lookup_ets({SessionId,<<"auth">>}) of 
        undefined ->
            CookieValue = case kvs:get(user,SessionUser) of
                {ok,User} ->
                    SS = lists:keyfind(n2o,1,User#user.tokens),
                    case SS of
                        {n2o,SessionId} -> SessionId;
                        _ -> new_cookie_value() end;
                _ -> new_cookie_value() end,
            Cookie = {{CookieValue,<<"auth">>},<<"/">>,now(),{TTL,Till},new},
            ets:insert(cookies,Cookie),
            wf:info(?MODULE,"Cookie New: ~p",[Cookie]), 
            Cookie;
        {{Session,Key},Path,Issued,{_TTL,_Till},Status} ->
            case expired(Issued,{_TTL,_Till}) of
                false ->
                    Cookie = {{Session,Key},Path,Issued,{_TTL,_Till},Status},
                    wf:info(?MODULE,"Cookie Same: ~p",[Cookie]),
                    Cookie;
                true -> Cookie = {{new_cookie_value(),<<"auth">>},<<"/">>,now(),{TTL,Till},new},
                    ets:insert(cookies,Cookie), 
                    wf:info(?MODULE,"Cookie Expired: ~p",[Cookie]),
                    Cookie end;
        _ -> error_logger:info_msg("Cookie Error"), skip
    end,
    {{ID,_},_,_,_,_} = SessionCookie,
    put(session_id,ID),
    wf:info(?MODULE,"State: ~p",[SessionCookie]),
    {ok, State, Ctx#context{session=SessionCookie}}.

expired(_Issued,{_TTL,Till}) -> false. % Till < calendar:now_to_datetime(now()).

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

cookie_expire(SecondsToLive) -> 
  Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
  httpd_util:rfc1123_date(DateTime).

ttl() -> 60*60*24*30.

session_id() -> get(session_id).
new_cookie_value() ->
    SessionKey = base64:encode(erlang:md5(term_to_binary({now(), make_ref()}))),
    wf:wire(wf:f("document.cookie='~s=~s; path=/; expires=~s';",
                [wf:to_list(session_cookie_name()),
                 wf:to_list(SessionKey),
                 cookie_expire(ttl())])),
    SessionKey.

new_cookie_value(SessionKey) ->
    wf:info(?MODULE,wf:f("document.cookie='~s=~s; path=/; expires=~s';",
                [wf:to_list(session_cookie_name()),
                 wf:to_list(SessionKey),
                 cookie_expire(ttl())]),[]),
    SessionKey.

new_state() -> #state{unique=new_cookie_value()}.
session_cookie_name() -> <<"n2o-sid">>.
set_value(Key, Value) -> ets:insert(cookies,{{session_id(),Key},Value}), Value.
get_value(Key, DefaultValue) -> 
    Res = case lookup_ets({session_id(),Key}) of
               undefined -> DefaultValue;
               {_,Value} -> Value end,
    wf:info(?MODULE,"Session Lookup Key ~p Value ~p",[Key,Res]),
    Res.

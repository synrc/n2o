-module(n2o_session_handler).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(session_handler).
-export([init/2, finish/2, get_value/2, set_value/2]).
-compile(export_all).
-record(state, {unique, node}).

init2(State, Req) -> {ok, State, Req}.

init(State, Ctx) -> 
    C = n2o_cowboy:cookie(session_cookie_name(),Ctx#context.req),
    SessionId = case C of
                     undefined -> undefined;
                     A when is_list(A) -> list_to_binary(A);
                     _Else -> _Else end,
    TTL = 24 * 60 * 60, % 1 day TTL
    SessionCookie = case lookup_ets({SessionId,<<"auth">>}) of 
                 undefined -> Cookie = {{new_cookie_value(),<<"auth">>},<<"/">>,now(),TTL,new},
                              ets:insert(cookies,Cookie),
%                              error_logger:info_msg("Cookie New: ~p",[Cookie]),
                              Cookie;
                 {{Session,Key},Path,Issued,TTL,Status} -> case expired(Issued,TTL) of
                     false -> Cookie = {{Session,Key},Path,Issued,TTL,Status},
%                              error_logger:info_msg("Cookie Same: ~p",[Cookie]),
                              Cookie;
                      true -> Cookie = {{new_cookie_value(),<<"auth">>},<<"/">>,now(),TTL,new},
                              ets:insert(cookies,Cookie), 
%                              error_logger:info_msg("Cookie Expired: ~p",[Cookie]),
                              Cookie end;
                 _ -> error_logger:info_msg("Cookie Error"), skip
                      end,
%    error_logger:info_msg("State: ~p",[SessionCookie]),
    {ok, State, Ctx#context{session=SessionCookie}}.

expired(Issued,TTL) ->
    false.

finish(State, Ctx) -> 
%    error_logger:info_msg("Finish Cookie Set ~p",[{_Config,State}]),
    NewReq = case Ctx#context.session of
         {{Session,Key},Path,Issued,TTL,Status} -> 
%     error_logger:info_msg("Finish Cookie Set ~p",[{{Session,Key},Path,Issued,TTL,Status}]),
              n2o_cowboy:cookie(session_cookie_name(),Session,Path,TTL,Ctx#context.req);
%              put(req,New),
%              New;
%               skip;
         _ -> Ctx#context.req end,
    {ok, [], Ctx#context{req=NewReq}}.

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
%    error_logger:info_msg("Lookup ETS: ~p",[{Res,Key}]),
    case Res of 
         [] -> undefined;
         [Value] -> Value;
         Values -> Values end.

new_cookie_value() -> base64:encode(erlang:md5(term_to_binary({now(), make_ref()}))).
new_state() -> #state{unique=new_cookie_value()}.
session_cookie_name() -> <<"n2o-sid">>.
get_value(Key, DefaultValue) -> 
    Res = case lookup_ets({wf:cookie(session_cookie_name(),?REQ),Key}) of
               undefined -> DefaultValue;
               {_,Value} -> Value end,
%    error_logger:info_msg("Session Lookup Key ~p Value ~p",[Key,Res]),
    Res.
set_value(Key, Value) -> ets:insert(cookies,{{n2o_cowboy:cookie(session_cookie_name(),?REQ),Key},Value}), Value.

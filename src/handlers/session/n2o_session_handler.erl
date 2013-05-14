-module(n2o_session_handler).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(session_handler).
-export([init/2, finish/2, get_value/4, set_value/4, clear_all/2, session_id/2]).
-compile(export_all).
-record(state, {unique, node}).

init(_Config, _State) -> 
    SessionId = case wf:cookie(session_cookie_name()) of
                     undefined -> undefined;
                     A when is_list(A) -> list_to_binary(A);
                     _Else -> _Else end,
    TTL = 20,
    State = case lookup_ets({SessionId,<<"auth">>}) of 
                 undefined -> Cookie = {{new_cookie_value(),<<"auth">>},"/",now(),TTL,new},
                              ets:insert(cookies,Cookie),
                              error_logger:info_msg("Cookie New: ~p",[Cookie]),
                              Cookie;
                 {{Session,Key},Path,Issued,TTL,Status} -> case expired(Issued,TTL) of
                     false -> Cookie = {{Session,Key},Path,Issued,TTL,Status},
                              error_logger:info_msg("Cookie Same: ~p",[Cookie]),
                              Cookie;
                      true -> Cookie = {{new_cookie_value(),<<"auth">>},"/",now(),TTL,new},
                              ets:insert(cookies,Cookie), 
                              error_logger:info_msg("Cookie Expired: ~p",[Cookie]),
                              Cookie end;
                 _ -> error_logger:info_msg("Cookie Error") end,

    {ok, State}.

expired(Issued,TTL) ->
    false.

finish(_Config, State) -> 
    {{Session,Key},Path,Issued,TTL,Status} = State,
    wf:cookie(session_cookie_name(),binary_to_list(Session),Path,TTL),
    {ok, []}.

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
    case Res of 
         [] -> undefined;
         [Value] -> Value;
         Values -> Values end.

new_cookie_value() -> wf:pickle(erlang:md5(term_to_binary({now(), erlang:make_ref()}))).
new_state() -> #state{unique=new_cookie_value()}.
session_cookie_name() -> "n2o-cookie".
session_id(_Config, State) -> {ok, SessionId} = wf:hex_encode(State#state.unique), {ok, SessionId, State}.
clear_all(Config, State) -> {ok, State}.
get_value(Key, DefaultValue, Config, State) -> 
    Res = case lookup_ets({wf:cookie(session_cookie_name()),Key}) of
               undefined -> DefaultValue;
               Value -> Value end,
    error_logger:info_msg("Session Lookup Key ~p Value ~p",[Key,Res]),
    {ok, Res, State}.
set_value(Key, Value, Config, State) -> ets:insert(cookies,{{wf:cookie(session_cookie_name()),Key},Value}), {ok, Value, State}.

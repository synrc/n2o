-module(n2o_session_handler).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(session_handler).
-export([init/2, finish/2, get_value/4, set_value/4, clear_all/2, session_id/2]).
-record(state, {unique, node}).

init(_Config, _State) -> 
    % Get the session cookie and node...
    Cookie = wf:cookie(get_cookie_name()),
    State = case wf:depickle(Cookie) of
        undefined -> new_state();
        Other -> Other
    end,
    {ok, State}.

finish(_Config, State) -> 
    % Drop the session cookie...
    Timeout = wf:config_default(session_timeout, 20),
    wf:cookie(get_cookie_name(), wf:pickle(State), "/", Timeout),
    {ok, []}.

get_value(Key, DefaultValue, Config, State) -> 
    Value = get(Key),
    {ok, Value, State}.

set_value(Key, Value, Config, State) -> 
    OldValue = "implement_me",
    put(Key,Value),
    {ok, OldValue, State}.

clear_all(Config, State) -> 
    {ok, State}.

session_id(_Config, State) ->
    {ok, SessionId} = wf:hex_encode (State#state.unique),
    {ok, SessionId, State}.

%%% PRIVATE FUNCTIONS

get_cookie_name() -> "n2o-cookie".

new_state() ->
    Unique = erlang:md5(term_to_binary({now(), erlang:make_ref()})),
    #state { unique=Unique }.

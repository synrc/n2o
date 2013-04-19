% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% This is a "simple as possible" session handler. Unfortunately,
% due to time constraints, had to leave out some great code
% contributed by Dave Peticolas that fit Nitrogen sessions
% into a gen_server. My code below is far inferior. 
% Someone please make it better! - Rusty

-module (simple_session_handler).
-include_lib ("wf.hrl").
-behaviour (session_handler).
-export ([
    init/2, 
    finish/2,
    get_value/4, 
    set_value/4, 
    clear_all/2,
    session_id/2
]).
-record (state, {unique, node}).

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
    ok = wf:cookie(get_cookie_name(), wf:pickle(State), "/", Timeout),
    {ok, []}.

get_value(Key, DefaultValue, Config, State) -> 
    {ok, Pid} = get_session_pid(Config, State),
    Ref = make_ref(),
    Pid!{get_value, Key, self(), Ref},
    Value = receive 
        {ok, undefined, Ref} -> DefaultValue;
        {ok, Other, Ref} -> Other
    end,
    {ok, Value, State}.

set_value(Key, Value, Config, State) -> 
    {ok, Pid} = get_session_pid(Config, State),
    Ref = make_ref(),
    Pid!{set_value, Key, Value, self(), Ref},
    receive {ok, OldValue, Ref} -> ok end,	
    {ok, OldValue, State}.

clear_all(Config, State) -> 
    {ok, Pid} = get_session_pid(Config, State),
    Ref = make_ref(),
    Pid!{clear_all, self(), Ref},
    receive {ok, Ref} -> ok end,	
    {ok, State}.

session_id(_Config, State) ->
    {ok, SessionId} = wf:hex_encode (State#state.unique),
    {ok, SessionId, State}.

%%% PRIVATE FUNCTIONS

get_cookie_name() ->
    wf:config_default(cookie_name, "newcookie").

get_session_pid(_Config, State) ->
    Timeout = wf:config_default(session_timeout, 20),
    F = fun() -> session_loop([], Timeout) end,
    SessionTag = {session, State#state.unique},
    {ok, _Pid} = process_registry_handler:get_pid(SessionTag, F).

session_loop(Session, Timeout) ->
    %% Timeout in 10 if the session is empty...
    TimeoutMS = case Session == [] of
        true  -> 10 * 1000;
        false -> Timeout * 60 * 1000
    end,
    receive
        {get_value, Key, Pid, Ref} ->
            Value = case lists:keysearch(Key, 1, Session) of
                {value, {Key, V}} -> V;
                false -> undefined
            end,
            Pid!{ok, Value, Ref},
            session_loop(Session, Timeout);

        {set_value, Key, Value, Pid, Ref} ->
            OldValue = case lists:keysearch(Key, 1, Session) of
                {value, {Key, V}} -> V;
                false -> undefined
            end,
            Session1 = lists:keystore(Key, 1, Session, {Key, Value}),
            Pid!{ok, OldValue, Ref},
            session_loop(Session1, Timeout);			

        {clear_all, Pid, Ref} ->
            Pid!{ok, Ref},
            session_loop([], Timeout)	

    after TimeoutMS -> 
        exit(timed_out)
    end.

new_state() ->
    Unique = erlang:md5(term_to_binary({now(), erlang:make_ref()})),
    #state { unique=Unique }.

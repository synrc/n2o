-module(n2o_stream).
-description('N2O Stream Bridge to WebSocket or XHR channels').
-author('Loïc Hoguin').
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
	handler :: module(),
	handler_state :: term()
}).

-define(TIMEOUT, 60000). %% @todo Configurable.

%% HTTP.

init(Transport, Req, Opts) ->
	case cowboy_req:header(<<"upgrade">>, Req) of
		{undefined, Req2} ->
			{Method, Req3} = cowboy_req:method(Req2),
			init(Transport, Req3, Opts, Method);
		{Bin, Req2} when is_binary(Bin) ->
			case cowboy_bstr:to_lower(Bin) of
				<<"websocket">> ->
					{upgrade, protocol, cowboy_websocket};
				_Any ->
					{ok, Req3} = cowboy_req:reply(501, [], [], Req2),
					{shutdown, Req3, undefined}
			end
	end.

init(Transport, Req, Opts, <<"GET">>) ->
	Handler = n2o_proto,
    wf:info(?MODULE,"GET XHR POLL ~p~n",[self()]),
	State = #state{handler=Handler},
	case n2o_proto:init(Transport, Req, Opts, xhr) of
		{ok, Req2, HandlerState} ->
			Req3 = cowboy_req:compact(Req2),
			{loop, Req3, State#state{handler_state=HandlerState}, ?TIMEOUT, hibernate};
		{shutdown, Req2, HandlerState} ->
			{shutdown, Req2, State#state{handler_state=HandlerState}};
	    Else -> wf:info(?MODULE,"Unexpected return from GET XHR INIT ~p~n",[Else])
	end;
init(Transport, Req, Opts, <<"POST">>) ->
	Handler = n2o_proto,
    wf:info(?MODULE,"POST XHR INIT ~p~n",[self()]),
	State = #state{handler=Handler},
	case Handler:init(Transport, Req, Opts, xhr) of
		{reply, Req2, HandlerState} ->
		    {ok, Req2, State#state{handler_state=HandlerState}};
			%{loop, Req2, State#state{handler_state=HandlerState}, ?TIMEOUT, hibernate};
		{ok, Req2, HandlerState} ->
		    {ok, Req2, State#state{handler_state=HandlerState}};
		{shutdown, Req2, HandlerState} ->
		    {shutdown, Req2, State#state{handler_state=HandlerState}};
	    Else ->
	        wf:info(?MODULE,"Unexpected return from POST XHR INIT ~p~n",[Else])
	end;
init(Transport, Req, Opts, <<"OPTIONS">>) ->
	Handler = n2o_proto,
	State = #state{handler=Handler},
    wf:info(?MODULE,"OPTIONS XHR ~p~n",[cowboy_req:body(Req)]),
	case Handler:init(Transport, Req, Opts, once) of
		{ok, Req2, HandlerState} ->
			{ok, cowboy_req:compact(Req2), State#state{handler_state=HandlerState}};
	    {shutdown, Req2, HandlerState} ->
		    {shutdown, Req2, State#state{handler_state=HandlerState}}
	end;

init(_Transport, Req, _Opts, _Method) ->
	{ok, Req2} = cowboy_req:reply(405, [], [], Req),
	{shutdown, Req2, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	handle(Req2, State, Method).
handle(Req, State, <<"OPTIONS">>)->
    Headers = [
        {<<"Content-Type">>, <<"text/html; charset=utf-8">>},
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Headers">>, <<"X-Socket-Transport, Content-Type, x-requested-with, Accept">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>}
    ],
    {ok, Req1} = cowboy_req:reply(200, Headers, [], cowboy_req:compact(Req)),
    {ok, Req1, State};
handle(Req, State=#state{handler=Handler, handler_state=HandlerState}, <<"POST">>) ->
	Body=cowboy_req:body(Req),
    wf:info(?MODULE,"POST XHR ~p~n",[self()]),
	case Body of
		{ok, Data, Req2} ->
            Type = case Data of
                <<"N2O,",_/binary>> -> text;
                <<"PING">> -> text;
                _ -> binary end,
            wf:info(?MODULE,"POST XHR STREAM ~p~n",[{Type,Data}]),
			case Handler:stream({Type,Data}, Req2, HandlerState) of
				{ok, Req3, HandlerState2} ->
					{ok, Req3, State#state{handler_state=HandlerState2}};
				{reply, Reply, Req3, HandlerState2} ->
					{ok, Req4} = cowboy_req:reply(200, [], Reply, Req3),
					{ok, Req4, State#state{handler_state=HandlerState2}}
			end;
		{error, _} -> {ok, Req, State};
	    Else -> wf:info(?MODULE,"Unexpected return from POST XHR INIT ~p~n",[Else])
	end.

info(Message, Req, State=#state{handler=Handler, handler_state=HandlerState}) ->
	case Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{loop, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, Data, Req2, HandlerState2} ->
			{ok, Req3} = cowboy_req:reply(200, [], Data, Req2),
			{ok, Req3, State#state{handler_state=HandlerState2}}
	end.

terminate(_Reason, _Req, undefined) ->
    wf:info(?MODULE,"bullet_handler terminate",[]),
	ok;
terminate(_Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
    wf:info(?MODULE,"bullet_handler state terminate",[]),
	Handler:terminate(Req, HandlerState).

%% Websocket.

websocket_init(Transport, Req, Opts) ->
	Handler = n2o_proto,
	State = #state{handler=Handler},
	case Handler:init(Transport, Req, Opts, true) of
		{ok, Req2, HandlerState} ->
			Req3 = cowboy_req:compact(Req2),
			{ok, Req3, State#state{handler_state=HandlerState},
				?TIMEOUT, hibernate};
		{shutdown, Req2, _HandlerState} ->
			{shutdown, Req2}
	end.

websocket_handle(Data, Req, State=#state{handler=Handler, handler_state=HandlerState}) ->
	case Handler:stream(Data, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{ok, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, {binary, Reply}, Req2, HandlerState2} ->
%		        wf:info(?MODULE,"Bullet Handle Binary"),
			{reply, {binary, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate};
		{reply, Reply, Req2, HandlerState2} ->
			{reply, {text, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate}
	end;

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_info(Info, Req, State=#state{handler=Handler, handler_state=HandlerState}) ->
	case Handler:info(Info, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{ok, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, {binary, Reply}, Req2, HandlerState2} ->
			{reply, {binary, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate};
		{reply, Reply, Req2, HandlerState2} ->
			{reply, {text, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate}
	end.

websocket_terminate(_Reason, Req,#state{handler=Handler, handler_state=HandlerState}) ->
	Handler:terminate(Req, HandlerState).

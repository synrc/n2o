%% Feel free to use, reuse and abuse the code in this file.

%% @doc Stream handler for clock synchronizing.
-module(n2o_bullet).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    wf_context:init_context(RequestBridge,ResponseBridge),
    wf_core:call_init_on_handlers(),
   {ok, Req, undefined_state}.
% io:format("bullet init~n"),
% _ = erlang:send_after(?PERIOD, self(), refresh),
% {ok, Req, undefined}.

stream(<<"ping">>, Req, State) ->
	io:format("ping received~n"),
	{reply, <<"pong">>, Req, State};
stream({text,Data}, Req, State) ->    
    error_logger:info_msg("Text Received ~p",[Data]),    
    self() ! Data,     
    {ok, Req,State};
stream({binary,Info}, Req, State) ->
    error_logger:info_msg("Binary Received: ~p",[Info]),    
    Pro = binary_to_term(Info,[safe]),
    Pickled = proplists:get_value(pickle,Pro),
    Linked = proplists:get_value(linked,Pro),
    Api = proplists:get_value(extras,Pro),
    error_logger:info_msg("Extras  ~p~n",[Api]),
    Depickled = wf_pickle:depickle(Pickled),
    error_logger:info_msg("Depickled  ~p~n",[Depickled]),
    case Api of
         <<"api">> -> {event_context,_,Args,_,_,_} = Depickled,
                      action_api:event(Args,Linked);       
         _ ->  lists:map(fun({K,V})->put(K,V)end,Linked) end,
                error_logger:info_msg("Depickled  ~p~n",[Depickled]),
                case Depickled of  
                     {event_context,Module,Parameter,_,_,_} -> Res = Module:event(Parameter);
                                            %              error_logger:info_msg("Event Result ~p~n",[Res]);
                                                       _ -> error_logger:info_msg("Unknown Event") end,
               {ok,Render} = wf_render_actions:render_actions(wf_context:actions()),
               wf_context:clear_actions(),    
               error_logger:info_msg("Render: ~p~n",[Render]),    
               error_logger:info_msg("Cookies: ~p~n",[wf:cookies()]),    
               error_logger:info_msg("Session Data: ~p~n",[wf:session(id)]),    
               error_logger:info_msg("Headers: ~p~n",[wf:headers()]),    
    {reply,lists:flatten(Render), Req, State};
%	io:format("stream received ~s~n", [Data]),
%	{ok, Req, State}.
stream(Data, Req, State) ->    
     error_logger:info_msg("Data Received ~p",[Data]),    
     self() ! Data,
    {ok, Req,State}.

info(Pro, Req, State) ->
    error_logger:info_msg("WSINFO: ~p",[Pro]),    
    Res = case Pro of
         {flush,Actions} -> {ok,Render} = wf_render_actions:render_actions(Actions),
 %                        error_logger:info_msg("Render: ~p",[Render]),
                            lists:flatten(Render);
          <<"N2O">> ->     error_logger:info_msg("N2O WS INIT ACK: ~p",[wf_context:page_module()]),
                          (wf_context:page_module()):event(init),
                         {ok,Render} = wf_render_actions:render_actions(wf_context:actions()),
                             wf_context:clear_actions(),
                            lists:flatten(Render);
          Unknown ->     error_logger:info_msg("Unknown: ~p",[Unknown]),
                           "OK"
                     end,
    error_logger:info_msg("Res: ~p",[Res]),
    {reply, Res, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.

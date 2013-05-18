-module(n2o_bullet).
-author('Maxim Sokhatsky').
-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
    put(req,Req),
    put(actions,[]),
    Handlers = wf_context:init_context(),
    wf_core:init(Handlers),
    {ok, Req, undefined_state}.

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
%    error_logger:info_msg("Module  ~p~n",[Module]),
    error_logger:info_msg("Extras  ~p~n",[Api]),
    error_logger:info_msg("Pickled  ~p~n",[Pickled]),
%    Decode = wf_event:jsonx_decoder(),
%    Depickled = Decode(Pickled), 
    Depickled = wf_pickle:depickle(Pickled),
    error_logger:info_msg("Depickled  ~p~n",[Depickled]),
    case Api of
         <<"api">> -> {event_context,_,Args,_,_,_} = Depickled,
                      action_api:event(Args,Linked);       
         _ ->  lists:map(fun({K,V})->put(K,V)end,Linked) end,
                case Depickled of  
                     {event_context,Module,Parameter,_,_,_} -> Res = Module:event(Parameter);
                                                          _ -> error_logger:info_msg("Unknown Event") end,
               Render = wf_render_actions:render_actions(get(actions)),
    wf_context:clear_actions(),
    {reply,Render, Req, State};
stream(Data, Req, State) ->    
     error_logger:info_msg("Data Received ~p",[Data]),    
     self() ! Data,
    {ok, Req,State}.

info(Pro, Req, State) ->
    Res = case Pro of
         {flush,Actions} -> wf_render_actions:render_actions(Actions);
          <<"N2O,",Rest/binary>> -> (get(page_module)):event(init),
                        Pid = list_to_pid(binary_to_list(Rest)),
                        Pid ! {'N2O',self()},
                        InitActions = receive Actions -> Actions end,
                        RenderInit = wf_render_actions:render_actions(InitActions),
                        RenderOther = wf_render_actions:render_actions(get(actions)),
                        RenderInit ++ RenderOther;
          Unknown ->     <<"OK">> end,
    wf_context:clear_actions(),
    {reply, Res, Req, State}.

terminate(_Req, _State) ->
    io:format("bullet terminate~n"),
    ok.

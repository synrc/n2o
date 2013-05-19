-module(n2o_bullet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
    put(req,Req),
    put(actions,[]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_core:fold(init,Ctx#context.handlers,Ctx),
    put(page_module,NewCtx#context.module),
    {ok, NewCtx#context.req, NewCtx}.

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
%    error_logger:info_msg("Extras  ~p~n",[Api]),
%    error_logger:info_msg("Pickled  ~p~n",[Pickled]),
    Depickled = wf_pickle:depickle(Pickled),
    error_logger:info_msg("Depickled  ~p~n",[Depickled]),
    case Api of
         <<"api">> -> #ev{payload=Args} = Depickled,
                      action_api:event(Args,Linked,State);
         _ ->  lists:map(fun({K,V})->put(K,V)end,Linked) end,
                case Depickled of
                     #ev{module=Module,payload=Parameter} -> Res = Module:event(Parameter);
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
          <<"N2O,",Rest/binary>> -> (State#context.module):event(init),
                        Pid = list_to_pid(binary_to_list(Rest)),
                        Pid ! {'N2O',self()},
                        InitActions = receive Actions -> Actions end,
%                        error_logger:info_msg("Transition Actions: ~p",[InitActions]),
                        RenderInit = wf_render_actions:render_actions(InitActions),
                        RenderOther = wf_render_actions:render_actions(get(actions)),
                        RenderInit ++ RenderOther;
          Unknown ->     <<"OK">> end,
    wf_context:clear_actions(),
    {reply, Res, Req, State}.

terminate(_Req, _State) ->
    io:format("bullet terminate~n"),
    ok.

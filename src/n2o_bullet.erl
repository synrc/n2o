-module(n2o_bullet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
    put(actions,[]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_core:fold(init,Ctx#context.handlers,Ctx),
    wf_context:context(NewCtx),
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
    Depickled = wf_pickle:depickle(Pickled),
    case Depickled of
        #ev{module=Module,name=Function,payload=Parameter,trigger=Trigger} ->
            case Function of 
                 control_event   -> lists:map(fun({K,V})-> put(K,V) end,Linked),
                                    Module:Function(Trigger, Parameter);
                 api_event       -> Module:Function(Parameter,Linked,State);
                 event           -> lists:map(fun({K,V})-> put(K,V) end,Linked),
                                    Module:Function(Parameter);
                 UserCustomEvent -> Module:Function(Parameter,Trigger,State) end;
          _ -> error_logger:error_msg("N2O allows only #ev{} events") end,

    Actions = get(actions),
    Render = wf_core:render(Actions),

    wf_context:clear_actions(),
    GenActions = get(actions),
    RenderGenActions = wf_core:render(GenActions),
    wf_context:clear_actions(),

    {reply,[Render,RenderGenActions], Req, State};
stream(Data, Req, State) ->    
    error_logger:info_msg("Data Received ~p",[Data]),    
    self() ! Data,
    {ok, Req,State}.

info(Pro, Req, State) ->
    Render =  case Pro of
                {flush,Actions} -> 
                    error_logger:info_msg("Comet Actions: ~p",[Actions]),
                    wf_core:render(Actions);
                <<"N2O,",Rest/binary>> ->
                    Module = State#context.module, Module:event(init),
                    Pid = list_to_pid(binary_to_list(Rest)),
                    X = Pid ! {'N2O',self()},
                    InitActions = receive Actions ->
                        RenderInit = wf_core:render(Actions),
                        RenderOther = wf_core:render(get(actions)),
                        Y = [RenderInit, RenderOther],
                        ets:insert(actions,{Module,Y}),
                        Y
                    after 100 -> [{Module,A}] = ets:lookup(actions,Module), A end,
                    InitActions;
            Unknown -> <<"OK">> end,

    wf_context:clear_actions(),
    GenActions = get(actions),
    RenderGenActions = wf_core:render(GenActions),
    wf_context:clear_actions(),

    {reply, [Render,RenderGenActions], Req, State}.

terminate(_Req, _State) ->
    error_logger:info_msg("Bullet Terminated~n"),
    ok.

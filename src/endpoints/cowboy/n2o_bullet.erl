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
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, <<"*">>, NewCtx#context.req),
    {ok, Req1, NewCtx}.

stream(<<"ping">>, Req, State) ->
    wf:info("ping received~n"),
    {reply, <<"pong">>, Req, State};
stream({text,Data}, Req, State) ->
    % wf:info("Text Received ~p",[Data]),
    self() ! Data,
    {ok, Req,State};
stream({binary,Info}, Req, State) ->
    % wf:info("Binary Received: ~p",[Info]),
    Pro = binary_to_term(Info,[safe]),
    Pickled = proplists:get_value(pickle,Pro),
    Linked = proplists:get_value(linked,Pro),
    Depickled = wf:depickle(Pickled),
    % wf:info("Depickled: ~p",[Depickled]),
    case Depickled of
        #ev{module=Module,name=Function,payload=Parameter,trigger=Trigger} ->
            case Function of 
                control_event   -> lists:map(fun({K,V})-> put(K,V) end,Linked),
                                   Module:Function(Trigger, Parameter);
                api_event       -> Module:Function(Parameter,Linked,State);
                event           -> lists:map(fun({K,V})-> put(K,V) end,Linked),
                                   Module:Function(Parameter);
                UserCustomEvent -> Module:Function(Parameter,Trigger,State) end;
          _ -> wf:error("N2O allows only #ev{} events") end,

    Actions = get(actions),
    wf_context:clear_actions(),
    Render = wf:render(Actions),

    GenActions = get(actions),
    RenderGenActions = wf:render(GenActions),
    wf_context:clear_actions(),

    {reply, [Render,RenderGenActions], Req, State};
stream(Data, Req, State) ->
    wf:info("Data Received ~p",[Data]),
    self() ! Data,
    {ok, Req,State}.

info(Pro, Req, State) ->
    Render = case Pro of
        {flush,Actions} ->
            % error_logger:info_msg("Comet Actions: ~p",[Actions]),
            wf:render(Actions);
        <<"N2O,",Rest/binary>> ->
            Module = State#context.module, Module:event(init),
            InitActions = get(actions),
            wf_context:clear_actions(),
            Pid = wf:depickle(Rest),
            X = Pid ! {'N2O',self()},
            R = receive Actions ->
                RenderInit = wf:render(InitActions),
                InitGenActions = get(actions),
                RenderInitGenActions = wf:render(InitGenActions),
                wf_context:clear_actions(),
                RenderPage = wf:render(Actions),
                [RenderInit, RenderPage, RenderInitGenActions]
            after 100 ->
                QS = element(14, Req),
                wf:redirect(case QS of <<>> -> ""; _ -> "" ++ "?" ++ wf:to_list(QS) end),
                []
            end, R;
        <<"PING">> -> [];
        Unknown ->
            M = State#context.module,
            M:event(Unknown),
            Actions = get(actions),
            wf_context:clear_actions(),
            wf:render(Actions) end,
    GenActions = get(actions),
    wf_context:clear_actions(),
    RenderGenActions = wf:render(GenActions),
    wf_context:clear_actions(),
    {reply, [Render,RenderGenActions], Req, State}.

terminate(_Req, _State) ->
    % wf:info("Bullet Terminated~n"),
    ok.

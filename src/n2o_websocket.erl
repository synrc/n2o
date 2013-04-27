-module(n2o_websocket).
-behaviour(cowboy_websocket_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp,http}, _Req, _Opt) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opt) ->
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, Req),    
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    wf_context:init_context(RequestBridge,ResponseBridge),
    X = wf_context:context(),
    gproc:reg({p,l, main_room}),
    {ok, Req, undefined_state}.
websocket_handle({text,Data}, Req, State) ->
    gproc:send({p,l,main_room},Data),
    {ok, Req,State};
websocket_handle({binary,Info}, Req, State) -> 
    Pro = binary_to_term(Info),
    Pickled = proplists:get_value(pickle,Pro),
    Depickled = wf_pickle:depickle(Pickled),
    case Depickled of
         {event_context,Module,Parameter,_,_,_} ->
              Res = Module:event(Parameter),
              error_logger:info_msg("Event Result ~p~n",[Res]);
         _ -> error_logger:info_msg("Unknown Event") end,
    {ok,Render} = wf_render_actions:render_actions(wf_context:actions()), 
    wf_context:clear_actions(),
    error_logger:info_msg("Render: ~p~n",[Render]),
    gproc:send({p,l,main_room},Pro),
    {reply,{binary,term_to_binary(lists:flatten(Render))}, Req, State};
websocket_handle(_Any, Req, State) -> {ok, Req, State}.
websocket_info(Pro, Req, State) ->
    {reply, {binary,[term_to_binary(Pro)]}, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> 
    error_logger:info_msg("Terminate WS ~p~n",[_Reason]),
    ok.

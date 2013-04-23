-module(ws_chat).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp,http}, _Req, _Opt) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opt) -> gproc:reg({p,l, main_room}), {ok, Req, undefined_state}.
websocket_handle({text,Data}, Req, State) -> 
   error_logger:info_msg("WSPID: ~p Redirect Text ~p to Room ~n",[self(),Data]),
   gproc:send({p,l,main_room},Data),
   {ok, Req,State};
websocket_handle({binary,Info}, Req, State) -> 
    error_logger:info_msg("~p",[Info]),
    Pro = binary_to_term(Info),
    Pickled = proplists:get_value(pickle,Pro),
    Depickled = wf_pickle:depickle(Pickled),
    case Depickled of
         {event_context,Module,Parameter,_,_,_} -> 
              error_logger:info_msg("Before Dispatch"),
              A = Module:event(Parameter), 
              error_logger:info_msg("After ~p~n",[A]);
         _ -> error_logger:info_msg("Unknown Event") end,
   gproc:send({p,l,main_room},Pro),
   {ok, Req,State};
websocket_handle(_Any, Req, State) -> {ok, Req, State}.
websocket_info(Pro, Req, State) -> 
   {reply, {text,lists:flatten(io_lib:format("~p",[Pro]))}, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

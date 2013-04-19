-module(ws_chat).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp,http}, _Req, _Opt) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opt) -> gproc:reg({p,l, main_room}), {ok, Req, undefined_state}.
websocket_handle({text,Data}, Req, State) -> 
   error_logger:info_msg("WSPID: ~p~n",[self()]),
   gproc:send({p,l,main_room},Data),
   {ok, Req,State};
websocket_handle(_Any, Req, State) -> {ok, Req, State}.
websocket_info(X, Req, State) -> 
    <<H:8,Info/binary>> = X,
   error_logger:info_msg("~p",[Info]),
    Data1 = binary_to_term(Info),
   error_logger:info_msg("~p",[Data1]),
   {reply, {text,lists:flatten(io_lib:format("~p",[Data1]))}, Req, State}.
websocket_terminate(_Reason, _Req, _State) -> ok.
lhdaahdbahdcaj
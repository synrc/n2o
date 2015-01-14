-module(config).
-compile(export_all).

log_modules() ->
  [
%   n2o_websocket,
%   n2o_query,
%   n2o_bullet,
%   n2o_nitrogen,
%   n2o_dynroute,
%   n2o_client,
    users,
    login,
    index,
    index_rails
  ].

websocket_port() -> {ws, 8000, wss, 443}.

info() ->  spawn(fun()-> wf:info(index,"~p",[mnesia:info()]) end).

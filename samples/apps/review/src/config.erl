-module(config).
-compile(export_all).

log_modules() ->
  [
%   n2o_websocket,
%   n2o_query,
%   n2o_bullet,
%   n2o_nitrogen,
%   n2o_dynroute,
    n2o_client,
    login,
    index,
    index_rails
  ].

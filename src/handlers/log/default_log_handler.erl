-module(default_log_handler).
-author('Rusty Klophaus').
-behaviour (log_handler).
-export([init/2, finish/2, info/3, warning/3, error/3]).

init(_Config, State) -> {ok, State}.
finish(_Config, State) -> {ok, State}.
info(S, _Config, State) -> error_logger:info_msg([S, "\n"]), {ok, State}.
warning(S, _Config, State) -> error_logger:warning_msg([S, "\n"]), {ok, State}.
error(S, _Config, State) ->  error_logger:error_msg([S, "\n"]), {ok, State}.

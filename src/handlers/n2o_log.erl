-module(n2o_log).
-author('Roman Gladkov').
-export([info/3, warning/3, error/3]).


info(Module, String, Args) ->
    error_logger:info_msg(format_message(Module, String), Args).

warning(Module, String, Args) ->
    error_logger:warning_msg(format_message(Module, String), Args).

error(Module, String, Args) ->
    error_logger:error_msg(format_message(Module, String), Args).


format_message(Module, String) ->
    wf:to_list([Module, ":", String, "~n"]).

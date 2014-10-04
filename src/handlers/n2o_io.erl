-module(n2o_io).
-author('Roman Gladkov').
-export([info/3, warning/3, error/3]).


info(Module, String, Args) ->
    io:format(format_message(Module, String), Args).

warning(Module, String, Args) ->
    io:format(format_message(Module, String), Args).

error(Module, String, Args) ->
    io:format(format_message(Module, String), Args).

format_message(Module, String) ->
    wf:to_list([Module, ":", String, "\n\r"]).

-module(n2o_io).
-description('N2O Console Logging Driver').
-export([info/2, warning/2, error/2, info/3, warning/3, error/3]).

info(M, R)            -> io:format(format_message(M, info, "~p~n"), [R]).
warning(M, R)         -> io:format(format_message(M, warning, "~p~n"), [R]).
error(M, R)           -> io:format(format_message(M, error, "~p~n"), [R]).

info(M, F, A)           -> io:format(format_message(M, info, F), A).
warning(M, F, A)        -> io:format(format_message(M, warning, F), A).
error(M, F, A)          -> io:format(format_message(M, error, F), A).

format_message(M, L, F) -> lists:concat([M, ":", L, ":", F, "\n"]).

-ifndef(N2O_ASYNC).
-define(N2O_ASYNC, true).

-include("n2o_core.hrl").

-spec start(#handler{}) -> {pid(),term()} | #error{}.
-spec stop(term(),atom()) -> #handler{} | #error{}.

-endif.

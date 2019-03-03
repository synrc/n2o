-ifndef(N2O_PI).
-define(N2O_PI, true).

-include("n2o_core.hrl").

-spec start(#pi{}) -> {pid(),term()} | #error{}.
-spec stop(term(),atom()) -> #pi{} | #error{}.

-endif.

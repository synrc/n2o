-ifndef(N2O_API).
-define(N2O_API, true).

-include("n2o_core.hrl").

-type memtable()  :: atom().

-spec encode(tuple()) -> binary().
-spec decode(binary()) -> tuple().
-spec session(term(),term()) -> term().
-spec session(term()) -> term().
-spec cache(memtable(),term(),term()) -> term().
-spec cache(memtable(),term()) -> term().

-endif.

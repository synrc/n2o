-include("wf.hrl").

% n2o types

-type name() :: atom() | binary() | string().
-type render() :: list() | binary() | tuple() | list(tuple()).
-type wire_answer() :: undefined | list(tuple()).
-type wiring() :: string() | tuple().

% update

-spec update(name(), render()) -> wire_answer().
-spec insert_top(name(), render()) -> wire_answer().
-spec insert_bottom(name(), render()) -> wire_answer().
-spec insert_before(name(), render()) -> wire_answer().
-spec insert_after(name(), render()) -> wire_answer().
-spec remove(name()) -> wire_answer().

% wire

-spec wire(wiring()) -> wire_answer().

% async

-spec async(name(),fun()) -> {pid(),{async,{name(),any()}}}.
-spec start(#handler{}) -> {pid(),{name(),any()}}.
-spec stop(name()) -> any().
-spec restart(name()) -> any().
-spec flush() -> any().
-spec flush(any()) -> any().

% pickle

-spec pickle(any()) -> binary().
-spec depickle(binary()) -> any().

% session

-spec session(name()) -> any().
-spec session(name(), any()) -> any().
-spec user() -> any().
-spec user(any()) -> any().

% mq

-spec send(name(), any()) -> {ok,pid()}.
-spec reg(name(), any()) -> defined | undefined | any().
-spec unreg(name()) -> skip | undefined | any().

% query

-spec q(name()) -> any().
-spec qc(name()) -> any().
-spec qp(name()) -> any().


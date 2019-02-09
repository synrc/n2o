-ifndef(N2O_CORE).
-define(N2O_CORE, true).

-record(bert,    { data     :: term() }).
-record(json,    { data     :: term() }).
-record(binary,  { data     :: term() }).
-record(default, { data     :: term() }).
-record(ok,      { data     :: term() }).
-record(error,   { data     :: term() }).
-record(reply,   { msg      :: n2o(), req :: term(), ctx :: cx() } ).
-record(unknown, { msg      :: n2o(), req :: term(), ctx :: cx() } ).

-type n2o()       :: #bert{} | #json{} | #binary{} | #default{}.
-type cx()        :: #cx{}.
-type formatter() :: binary | json | bert | text | default | atom().
-type response()  :: { formatter(), binary() }.

-endif.

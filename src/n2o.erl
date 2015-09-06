-module(n2o).
-description('N2O OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1]).

tables()   -> [ cookies, actions, globals, caching ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,n2o},n2o,[]).
stop(_)    -> ok.
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, [] } }.

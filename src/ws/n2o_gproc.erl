-module(n2o_gproc).
-description('N2O GPROC MQ Backend').
-include_lib("n2o/include/n2o.hrl").
-export(?MESSAGE_API).

init() -> ok.
send(Pool, Message) -> gproc:send({p,l,Pool},Message).
reg(Pool) -> reg(Pool,undefined).
reg(Pool, Value) ->
    case get({pool,Pool}) of
         undefined -> gproc:reg({p,l,Pool},Value), put({pool,Pool},Pool);
         _Defined -> skip end.
unreg(Pool) ->
    case get({pool,Pool}) of
         undefined -> skip;
         _Defined -> gproc:unreg({p,l,Pool}), erase({pool,Pool}) end.

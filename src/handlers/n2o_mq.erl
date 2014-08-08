-module(n2o_mq).
-include_lib("n2o/include/wf.hrl").
-export(?MESSAGE_API).

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

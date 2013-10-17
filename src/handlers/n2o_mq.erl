-module(n2o_mq).
-include_lib("n2o/include/wf.hrl").
-export(?MESSAGE_API).

send(Pool, Message) -> gproc:send({p,l,Pool},Message).
reg(Pool) -> 
    Ctx = get({pool,Pool}),
    case Ctx of
         undefined -> gproc:reg({p,l,Pool}), put({pool,Pool},Pool);
         Defined -> skip end.

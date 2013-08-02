-module(n2o_gproc).
-compile(export_all).

send(Pool, Message) -> gproc:send({p,l,Pool},Message).
reg(Pool) -> 
    Ctx = get({pool,Pool}),
    case Ctx of
         undefined -> gproc:reg({p,l,Pool}), put({pool,Pool},Pool);
         Defined -> skip end.

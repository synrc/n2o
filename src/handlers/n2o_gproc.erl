-module(n2o_gproc).
-compile(export_all).

send(Pool, Message) -> gproc:send({p,l,Pool},Message).
reg(Pool) -> 
    Ctx = get(pool),
    case Ctx of
         undefined -> gproc:reg({p,l,Pool}), put(pool,Pool);
         Defined -> skip end.

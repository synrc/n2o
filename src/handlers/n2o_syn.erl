-module(n2o_syn).
-include_lib("n2o/include/wf.hrl").
-export(?MESSAGE_API).

send(Pool, Message) -> syn:publish(Pool,Message).
reg(Pool)           -> reg(Pool,self()).
reg(Pool,Pid)       -> syn:join(Pool,Pid).
unreg(Pool)         -> syn:leave(Pool,self()).

-module(n2o_xen).
-compile(export_all).

send(Pool, Message) -> global:send(Pool,Message).
reg(Pool) -> global:register_name(Pool,self()).

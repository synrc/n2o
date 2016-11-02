-module(n2o_syn).
-include_lib("n2o/include/wf.hrl").
-export(?MESSAGE_API).

send(Pool, Message) -> syn:publish(term_to_binary(Pool),Message).
reg(Pool) -> reg(Pool,undefined).
reg(Pool, Value) ->
    case get({pool,Pool}) of
         undefined -> syn:join(term_to_binary(Pool),self()),
                      put({pool,Pool},Pool);
         _Defined -> skip end.
unreg(Pool) ->
    case get({pool,Pool}) of
         undefined -> skip;
         _Defined -> syn:leave(term_to_binary(Pool), self()), 
                     erase({pool,Pool}) end.

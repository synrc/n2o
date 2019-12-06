-module(n2o_syn).
-description('N2O SYN MQ Backend').
-include("n2o.hrl").
-export(?MESSAGE_API).

init() -> ok.
send(Pool, Message) -> syn:publish(term_to_binary(Pool),Message).
reg(Pool) -> reg(Pool,undefined).
reg(Pool, _Value) ->
    case get({pool,Pool}) of
         undefined -> syn:join(term_to_binary(Pool),self()),
                      put({pool,Pool},Pool);
         _Defined -> skip end.
unreg(Pool) ->
    case get({pool,Pool}) of
         undefined -> skip;
         _Defined -> syn:leave(term_to_binary(Pool), self()),
                     erase({pool,Pool}) end.

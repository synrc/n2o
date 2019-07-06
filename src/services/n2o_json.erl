-module(n2o_json).
-description('N2O JSON Formatter').
-include("n2o.hrl").
-export([encode/1,decode/1]).

encode({_Io,Eval,Data}) ->
    jsone:encode([{t,104},{v,[
                 [{t,100},{v,io}],
                 [{t,109},{v,Eval}],
                 [{t,109},{v,Data}]]}]);

encode({Atom,Data}) ->
    jsone:encode([{t,104},{v,[
                 [{t,100},{v,Atom}],
                 [{t,109},{v,Data}]]}]).

decode(Bin) -> jsone:decode(Bin).

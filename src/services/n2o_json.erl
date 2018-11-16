-module(n2o_json).
-descrpition('N2O JSON Formatter').
-export([encode/1,decode/1]).

encode({Io,Eval,Data}) ->
    n2o:info(?MODULE,"{~p,_,_}: ~tp~n",[Io,Eval]),
    jsone:encode([{t,104},{v,[
                 [{t,100},{v,io}],
                 [{t,109},{v,Eval}],
                 [{t,109},{v,Data}]]}]);

encode({Atom,Data}) ->
    n2o:info(?MODULE,"{~p,_}:~tp~n",[Atom,Data]),
    jsone:encode([{t,104},{v,[
                 [{t,100},{v,Atom}],
                 [{t,109},{v,Data}]]}]).

decode(Bin) -> jsone:decode(Bin).

-module(n2o_igor).
-description("Module Merger").
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, _Options) ->
    SearchPath = "src/protos",
    put(igor,[]),
    File = [ F || F={attribute,_,file,_} <- Forms],
    Module = [ M || M={attribute,_,module,_} <- Forms],
    Modules = [ Module || {attribute,_,n2o,Module} <- Forms],
    X = lists:flatten([ begin {ok,R} = epp:parse_file(lists:concat([SearchPath,"/",M,".erl"]), [], []),
                R end || M <- Modules ]),
    Y = lists:flatten(
        lists:map(
          fun ({_,_,file,{N,_}}=F) -> case filename:extension(N) of ".erl" -> []; _ -> F end;
              ({_,_,file,{N,_}}=F) -> case filename:extension(N) of ".hrl" -> case get(N) of
                 undefined -> put(N,F), F;
                         _ -> [] end end;
              ({_,_,module,_}) -> [];
              ({_,_,export,[{info,3}]}) -> [];
              ({A,B,export,List}) -> {A,B,export,List--[{info,3}]};
              ({_,_,description,_}) -> [];
              ({function,_,info,3,F}) -> case get(igor) of
                   [] -> put(igor,F);
                   Saved -> put(igor,Saved ++ F) end, [];
              ({eof,_}) -> [];
              (Y) -> Y end, X)),
    {Exports,Body} = lists:partition(fun({_,_,export,_}) -> true; (_) -> false end, Y),
    Exp  = lists:usort(Exports) ++ [{attribute,3,export,[{info,3}]}],
    Lst  = lists:usort(Body),
    {Term,Clauses} = lists:partition(fun
       ({clause,_,[{var,_,_},{var,_,_},{var,_,_}],[],_}) -> true;
       (X) -> false end, get(igor)),
    Igor = [{function,100,info,3,Clauses ++ [hd(Term)]}],
    io:format("Forms: ~p~n",[Lst]),
    io:format("Clauses: ~p~n",[Clauses]),
    io:format("Module: ~p~n",[Module]),
    io:format("File: ~p~n",[File]),
    io:format("Exp: ~p~n",[Exp]),
    io:format("Term: ~p~n",[Term]),
    Module ++ File ++ Exp ++ Lst ++ Igor ++ [{eof,1000}].


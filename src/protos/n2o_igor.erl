-module(n2o_igor).
-description("Module Merger").
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    SearchPath = "src/protos",
    put(igor,[]),

    {_,_,_,{Name,Arity}} = lists:keyfind(entry,3,Forms),
    File    = [ F || F={attribute,_,file,_}   <- Forms],
    Module  = [ M || M={attribute,_,module,_} <- Forms],
    Modules = [ M ||   {attribute,_,proto,M}  <- Forms],

    X = lists:flatten([
     begin
       {ok,R} = epp:parse_file(lists:concat([SearchPath,"/",M,".erl"]), [], []),
       R
     end || M <- Modules ]),

    Y = lists:flatten(
        lists:map(
          fun ({_,_,file,{N,_}}=F) ->
              case filename:extension(N) of
                   ".erl" -> [];
                   ".hrl" -> case get(N) of
                     undefined -> put(N,F), F;
                             _ -> [] end  end;
              ({_,_,module,_}) -> [];
              ({_,_,export,[{N,A}]}) when N==Name, A==Arity -> [];
              ({A,B,export,List}) -> {A,B,export,List--[{Name,Arity}]};
              ({_,_,description,_}) -> [];
              ({function,_,N,A,F}) when Name==N, Arity==A -> case get(igor) of
                   [] -> put(igor,F);
                   Saved -> put(igor,Saved ++ F) end, [];
              ({eof,_}) -> [];
              (Y) -> Y end, X)),

    {Exports,Body} = lists:partition(fun({_,_,export,_}) -> true; (_) -> false end, Y),
    Exp  = lists:usort(Exports) ++ [{attribute,3,export,[{Name,Arity}]}],
    Lst  = lists:usort(Body),
    {Term,Clauses} = lists:partition(fun
       ({clause,_,[{var,_,_},{var,_,_},{var,_,_}],[],_}) -> true;
       (_) -> false end, get(igor)),
    Igor = [{function,100,Name,Arity,Clauses ++ [hd(Term)]}],

    Module ++ File ++ Exp ++ Lst ++ Igor ++ [{eof,1000}].


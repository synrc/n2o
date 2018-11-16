#!/usr/bin/env escript
-module(genbert).
-compile([export_all]).

object(D)   ->
    case crypto:rand_uniform(0,5) of
         0 -> tuple(D);
         1 -> bin(D);
         2 -> list(D);
         3 -> bytes(D);
         4 -> atom(D) end.

uni(1) -> rnd(0,16#7F);
uni(2) -> rnd(16#80,16#7FF);
uni(3) -> [rnd(16#800,16#D7FF),rnd(16#E000,16#FFFD)];
uni(4) -> rnd(16#10000,16#10FFFF).
utf8() -> [uni(X)||X<-lists:seq(1,3)].
unicode(0,Acc) -> lists:flatten(Acc);
unicode(N,Acc) -> unicode(N-1, [utf8()|Acc]).

size()     -> 20.
rnd(A)     -> rnd(1,A).
rnd(L,H)   -> crypto:rand_uniform(L,H).
list(2)    -> [];
list(D)    -> [ object(D+1) || _<- lists:seq(1,size()-D) ].
tuple(D)   -> list_to_tuple(list(D)).
bin(D)     -> list_to_binary(bytes(D)).
bytes(_)   -> latin(rnd(size()),[]).
atom(D)    -> list_to_atom(bytes(D)).
latin(0,A) -> A;
latin(N,A) -> latin(N-1, [rnd(size())+96|A]).
main(_)    -> [ io:format("~w\n",[binary_to_list(term_to_binary(tuple(0)))]) || _ <- lists:seq(1,size()) ].

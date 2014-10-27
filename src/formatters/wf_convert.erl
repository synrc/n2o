-module (wf_convert).
-author('Rusty Klophaus').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

-define(IS_STRING(Term), (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

%%% CONVERSION %%%

clean_lower(L) -> string:strip(string:to_lower(to_list(L))).

to_list(undefined) -> [];
to_list(L) when ?IS_STRING(L) -> L;
to_list(L) when is_list(L) ->
    SubLists = [inner_to_list(X) || X <- L],
    lists:flatten(SubLists);
to_list(A) -> inner_to_list(A).
inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(F) when is_float(F) -> 
	case F == round(F) of
		true -> inner_to_list(round(F));
		false -> n2o_mochinum:digits(F)
	end;
inner_to_list(L) when is_tuple(L) -> lists:flatten(io_lib:format("~p", [L]));
inner_to_list(L) when is_list(L) -> L.

to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> to_atom(binary_to_list(B));
to_atom(I) when is_integer(I) -> to_atom(integer_to_list(I));
to_atom(F) when is_float(F) -> to_atom(n2o_mochinum:digits(F));
to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(list_to_binary(L))).

to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> to_binary(n2o_mochinum:digits(F));
to_binary(L) when is_list(L) ->  iolist_to_binary(L). % unicode:characters_to_binary(L).

to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer([]) -> 0;
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(F) when is_float(F) -> round(F).

%%% TO STRING LIST %%%

%% @doc
%% Convert the following forms into a list of strings...
%% 	- atom
%%  - [atom, atom, ...]
%%  - "String"
%%  - "String, String, ..."
%%  - "String String ..."
%%  - [atom, "String", ...]
to_string_list(L) -> to_string_list(L, []).
to_string_list([], Acc) -> Acc;
to_string_list(undefined, Acc) -> Acc;
to_string_list(L, Acc) when is_atom(L) ->
    [atom_to_list(L)|Acc];
to_string_list(L, Acc) when ?IS_STRING(L) ->
    string:tokens(L, " ,") ++ Acc;
to_string_list(L, Acc) when is_binary(L) ->
    [binary_to_list(L)|Acc];
to_string_list([H|T], Acc) ->
    to_string_list(T, to_string_list(H) ++ Acc).


%%% HTML ENCODE %%%

html_encode(L,Fun) when is_function(Fun) -> Fun(L);

html_encode(L,EncType) when is_atom(L) -> html_encode(wf:to_list(L),EncType);
html_encode(L,EncType) when is_integer(L) -> html_encode(integer_to_list(L),EncType);
html_encode(L,EncType) when is_float(L) -> html_encode(n2o_mochinum:digits(L),EncType);

html_encode(L, false) -> L; %wf:to_list(lists:flatten([L]));
html_encode(L, true) -> L; %html_encode(wf:to_list(lists:flatten([L])));
html_encode(L, whites) -> html_encode_whites(wf:to_list(lists:flatten([L]))).

html_encode([]) -> [];
html_encode([H|T]) ->
	case H of
		$< -> "&lt;" ++ html_encode(T);
		$> -> "&gt;" ++ html_encode(T);
		$" -> "&quot;" ++ html_encode(T);
		$' -> "&#39;" ++ html_encode(T);
		$& -> "&amp;" ++ html_encode(T);
		BigNum when is_integer(BigNum) andalso BigNum > 255 ->
			%% Any integers above 255 are converted to their HTML encode equivilant,
			%% Example: 7534 gets turned into &#7534;
			[$&,$# | wf:to_list(BigNum)] ++ ";" ++ html_encode(T);
		Tup when is_tuple(Tup) -> 
			throw({html_encode,encountered_tuple,Tup});
		_ -> [H|html_encode(T)]
	end.

html_encode_whites([]) -> [];
html_encode_whites([H|T]) ->
	case H of
		$\s -> "&nbsp;" ++ html_encode_whites(T);
		$\t -> "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ html_encode_whites(T);
		$< -> "&lt;" ++ html_encode_whites(T);
		$> -> "&gt;" ++ html_encode_whites(T);
		$" -> "&quot;" ++ html_encode_whites(T);
		$' -> "&#39;" ++ html_encode_whites(T);
		$& -> "&amp;" ++ html_encode_whites(T);
		$\n -> "<br>" ++ html_encode_whites(T);
		_ -> [H|html_encode_whites(T)]
	end.



%%% HEX ENCODE and HEX DECODE

hex_encode(Data) -> encode(Data, 16).
hex_decode(Data) -> decode(Data, 16).

encode(Data, Base) when is_binary(Data) -> encode(binary_to_list(Data), Base);
encode(Data, Base) when is_list(Data) ->
    F = fun(C) when is_integer(C) ->
        case erlang:integer_to_list(C, Base) of
            [C1, C2] -> <<C1, C2>>;
            [C1]     -> <<$0, C1>>;
            _        -> throw("Could not hex_encode the string.")
        end
    end,
    {ok, list_to_binary([F(I) || I <- Data])}.

decode(Data, Base) when is_binary(Data) -> decode(binary_to_list(Data), Base);
decode(Data, Base) when is_list(Data) ->
    {ok, list_to_binary(inner_decode(Data, Base))}.

inner_decode(Data, Base) when is_list(Data) ->
    case Data of
        [C1, C2|Rest] ->
            I = erlang:list_to_integer([C1, C2], Base),
            [I|inner_decode(Rest, Base)];

        [] ->
            [];

        _  ->
            throw("Could not hex_decode the string.")
    end.

%%% URL ENCODE/DECODE %%%

url_encode(S) -> quote_plus(S).
url_decode(S) -> unquote(S).

%%% ESCAPE JAVASCRIPT %%%

js_escape(undefined) -> [];
js_escape(Value) when is_list(Value) -> binary_to_list(js_escape(iolist_to_binary(Value)));
js_escape(Value) -> js_escape(Value, <<>>).
js_escape(<<"\\", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\\\">>);
js_escape(<<"\r", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\r">>);
js_escape(<<"\n", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\n">>);
js_escape(<<"\"", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\\"">>);
js_escape(<<"'",Rest/binary>>,Acc) -> js_escape(Rest, <<Acc/binary, "\\'">>);
js_escape(<<"<script", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "<scr\" + \"ipt">>);
js_escape(<<"script>", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "scr\" + \"ipt>">>);
js_escape(<<C, Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, C>>);
js_escape(<<>>, Acc) -> Acc.


%%% JOIN %%%
%% Erlang doesn't provide a short way to join lists of "things" with other things.
%% string:join is not applicable here and only works on strings

join([],_) ->
	[];
join([Item],_Delim) ->
	[Item];
join([Item|Items],Delim) ->
	[Item,Delim | join(Items,Delim)].

%%% CODE BELOW IS FROM MOCHIWEB %%%

%% This is the MIT license.
%%
%% Copyright (c) 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
        C =:= $_))).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Bin) when is_binary(Bin) ->
    quote_plus(binary_to_list(Bin));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec unquote(string() | binary()) -> string()
%% @doc Unquote a URL encoded string.
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).

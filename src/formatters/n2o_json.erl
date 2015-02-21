-module(n2o_json).
-author('Bob Ippolito').
-export([encoder/1, encode/1]).
-export([decoder/1, decode/1, decode/2]).

%% This is a macro to placate syntax highlighters..
-define(Q, $\").
-define(ADV_COL(S, N), S#decoder{offset=N+S#decoder.offset,
                                 column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{offset=1+S#decoder.offset,
                              column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{offset=1+S#decoder.offset,
                               column=1,
                               line=1+S#decoder.line}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).
-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_eep18_object() = {[{json_string(), json_term()}]}
%% @type json_iolist() = {json, iolist()}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object() | json_eep18_object() | json_iolist()

-record(encoder, {handler=null,
                  utf8=false}).

-record(decoder, {object_hook=null,
                  offset=0,
                  line=1,
                  column=1,
                  state=null}).

%% @spec encoder([encoder_option()]) -> function()
%% @doc Create an encoder/1 with the given options.
%% @type encoder_option() = handler_option() | utf8_option()
%% @type utf8_option() = boolean(). Emit unicode as utf8 (default - false)
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> json_encode(O, State) end.

%% @spec encode(json_term()) -> iolist()
%% @doc Encode the given as JSON to an iolist.
encode(Any) ->
    json_encode(Any, #encoder{}).

%% @spec decoder([decoder_option()]) -> function()
%% @doc Create a decoder/1 with the given options.
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> json_decode(O, State) end.

%% @spec decode(iolist(), [{format, proplist | eep18 | struct}]) -> json_term()
%% @doc Decode the given iolist to Erlang terms using the given object format
%%      for decoding, where proplist returns JSON objects as [{binary(), json_term()}]
%%      proplists, eep18 returns JSON objects as {[binary(), json_term()]}, and struct
%%      returns them as-is.
decode(S, Options) ->
    json_decode(S, parse_decoder_options(Options, #decoder{})).

%% @spec decode(iolist()) -> json_term()
%% @doc Decode the given iolist to Erlang terms.
decode(S) ->
    json_decode(S, #decoder{}).

%% Internal API

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler});
parse_encoder_options([{utf8, Switch} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{utf8=Switch}).

parse_decoder_options([], State) ->
    State;
parse_decoder_options([{object_hook, Hook} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{object_hook=Hook});
parse_decoder_options([{format, Format} | Rest], State)
  when Format =:= struct orelse Format =:= eep18 orelse Format =:= proplist ->
    parse_decoder_options(Rest, State#decoder{object_hook=Format}).

json_encode(true, _State) ->
    <<"true">>;
json_encode(false, _State) ->
    <<"false">>;
json_encode(null, _State) ->
    <<"null">>;
json_encode(I, _State) when is_integer(I) ->
    integer_to_list(I);
json_encode(F, _State) when is_float(F) ->
    n2o_mochinum:digits(F);
json_encode(S, State) when is_binary(S); is_atom(S) ->
    json_encode_string(S, State);
json_encode([{K, _}|_] = Props, State) when (K =/= struct andalso
                                             K =/= array andalso
                                             K =/= json) ->
    json_encode_proplist(Props, State);
json_encode({struct, Props}, State) when is_list(Props) ->
    json_encode_proplist(Props, State);
json_encode({Props}, State) when is_list(Props) ->
    json_encode_proplist(Props, State);
json_encode({}, State) ->
    json_encode_proplist([], State);
json_encode(Array, State) when is_list(Array) ->
    json_encode_array(Array, State);
json_encode({array, Array}, State) when is_list(Array) ->
    json_encode_array(Array, State);
json_encode({json, IoList}, _State) ->
    IoList;
json_encode(Bad, #encoder{handler=null}) ->
    exit({json_encode, {bad_term, Bad}});
json_encode(Bad, State=#encoder{handler=Handler}) ->
    json_encode(Handler(Bad), State).

json_encode_array([], _State) ->
    <<"[]">>;
json_encode_array(L, State) ->
    F = fun (O, Acc) ->
                [$,, json_encode(O, State) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_proplist([], _State) ->
    <<"{}">>;
json_encode_proplist(Props, State) ->
    F = fun ({K, V}, Acc) ->
                KS = json_encode_string(K, State),
                VS = json_encode(V, State),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A, State) when is_atom(A) ->
    L = atom_to_list(A),
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(L), State, [?Q])
    end;
json_encode_string(B, State) when is_binary(B) ->
    case json_bin_is_safe(B) of
        true ->
            [?Q, B, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(B), State, [?Q])
    end;
json_encode_string(I, _State) when is_integer(I) ->
    [?Q, integer_to_list(I), ?Q];
json_encode_string(L, State) when is_list(L) ->
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(L, State, [?Q])
    end.

json_string_is_safe([]) ->
    true;
json_string_is_safe([C | Rest]) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
            false;
        C when C < 16#7f ->
            json_string_is_safe(Rest);
        _ ->
            false
    end.

json_bin_is_safe(<<>>) ->
    true;
json_bin_is_safe(<<C, Rest/binary>>) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f ->
            false;
        C when C < 16#7f ->
            json_bin_is_safe(Rest)
    end.

json_encode_string_unicode([], _State, Acc) ->
    lists:reverse([$\" | Acc]);
json_encode_string_unicode([C | Cs], State, Acc) ->
    Acc1 = case C of
               ?Q ->
                   [?Q, $\\ | Acc];
               %% Escaping solidus is only useful when trying to protect
               %% against "</script>" injection attacks which are only
               %% possible when JSON is inserted into a HTML document
               %% in-line. mochijson2 does not protect you from this, so
               %% if you do insert directly into HTML then you need to
               %% uncomment the following case or escape the output of encode.
               %%
               %% $/ ->
               %%    [$/, $\\ | Acc];
               %%
               $\\ ->
                   [$\\, $\\ | Acc];
               $\b ->
                   [$b, $\\ | Acc];
               $\f ->
                   [$f, $\\ | Acc];
               $\n ->
                   [$n, $\\ | Acc];
               $\r ->
                   [$r, $\\ | Acc];
               $\t ->
                   [$t, $\\ | Acc];
               C when C >= 0, C < $\s ->
                   [unihex(C) | Acc];
               C when C >= 16#7f, C =< 16#10FFFF, State#encoder.utf8 ->
                   [xmerl_ucs:to_utf8(C) | Acc];
               C when  C >= 16#7f, C =< 16#10FFFF, not State#encoder.utf8 ->
                   [unihex(C) | Acc];
               C when C < 16#7f ->
                   [C | Acc];
               _ ->
                   exit({json_encode, {bad_char, C}})
           end,
    json_encode_string_unicode(Cs, State, Acc1).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

unihex(C) when C < 16#10000 ->
    <<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
    Digits = [hexdigit(D) || D <- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =< 16#10FFFF ->
    N = C - 16#10000,
    S1 = 16#d800 bor ((N bsr 10) band 16#3ff),
    S2 = 16#dc00 bor (N band 16#3ff),
    [unihex(S1), unihex(S2)].

json_decode(L, S) when is_list(L) ->
    json_decode(iolist_to_binary(L), S);
json_decode(B, S) ->
    {Res, S1} = decode1(B, S),
    {eof, _} = tokenize(B, S1#decoder{state=trim}),
    Res.

decode1(B, S=#decoder{state=null}) ->
    case tokenize(B, S#decoder{state=any}) of
        {{const, C}, S1} ->
            {C, S1};
        {start_array, S1} ->
            decode_array(B, S1);
        {start_object, S1} ->
            decode_object(B, S1)
    end.

make_object(V, #decoder{object_hook=N}) when N =:= null orelse N =:= struct ->
    V;
make_object({struct, P}, #decoder{object_hook=eep18}) ->
    {P};
make_object({struct, P}, #decoder{object_hook=proplist}) ->
    P;
make_object(V, #decoder{object_hook=Hook}) ->
    Hook(V).

decode_object(B, S) ->
    decode_object(B, S#decoder{state=key}, []).

decode_object(B, S=#decoder{state=key}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, S1#decoder{state=null}};
        {{const, K}, S1} ->
            {colon, S2} = tokenize(B, S1),
            {V, S3} = decode1(B, S2#decoder{state=null}),
            decode_object(B, S3#decoder{state=comma}, [{K, V} | Acc])
    end;
decode_object(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, S1#decoder{state=null}};
        {comma, S1} ->
            decode_object(B, S1#decoder{state=key}, Acc)
    end.

decode_array(B, S) ->
    decode_array(B, S#decoder{state=any}, []).

decode_array(B, S=#decoder{state=any}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {start_array, S1} ->
            {Array, S2} = decode_array(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {start_object, S1} ->
            {Array, S2} = decode_object(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {{const, Const}, S1} ->
            decode_array(B, S1#decoder{state=comma}, [Const | Acc])
    end;
decode_array(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {comma, S1} ->
            decode_array(B, S1#decoder{state=any}, Acc)
    end.

tokenize_string(B, S=#decoder{offset=O}) ->
    case tokenize_string_fast(B, O) of
        {escape, O1} ->
            Length = O1 - O,
            S1 = ?ADV_COL(S, Length),
            <<_:O/binary, Head:Length/binary, _/binary>> = B,
            tokenize_string(B, S1, lists:reverse(binary_to_list(Head)));
        O1 ->
            Length = O1 - O,
            <<_:O/binary, String:Length/binary, ?Q, _/binary>> = B,
            {{const, String}, ?ADV_COL(S, Length + 1)}
    end.

tokenize_string_fast(B, O) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            O;
        <<_:O/binary, $\\, _/binary>> ->
            {escape, O};
        <<_:O/binary, C1, _/binary>> when C1 < 128 ->
            tokenize_string_fast(B, 1 + O);
        <<_:O/binary, C1, C2, _/binary>> when C1 >= 194, C1 =< 223,
                C2 >= 128, C2 =< 191 ->
            tokenize_string_fast(B, 2 + O);
        <<_:O/binary, C1, C2, C3, _/binary>> when C1 >= 224, C1 =< 239,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191 ->
            tokenize_string_fast(B, 3 + O);
        <<_:O/binary, C1, C2, C3, C4, _/binary>> when C1 >= 240, C1 =< 244,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191,
                C4 >= 128, C4 =< 191 ->
            tokenize_string_fast(B, 4 + O);
        _ ->
            throw(invalid_utf8)
    end.

tokenize_string(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            {{const, iolist_to_binary(lists:reverse(Acc))}, ?INC_COL(S)};
        <<_:O/binary, "\\\"", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\" | Acc]);
        <<_:O/binary, "\\\\", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\\ | Acc]);
        <<_:O/binary, "\\/", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$/ | Acc]);
        <<_:O/binary, "\\b", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\b | Acc]);
        <<_:O/binary, "\\f", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\f | Acc]);
        <<_:O/binary, "\\n", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\n | Acc]);
        <<_:O/binary, "\\r", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\r | Acc]);
        <<_:O/binary, "\\t", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\t | Acc]);
        <<_:O/binary, "\\u", C3, C2, C1, C0, Rest/binary>> ->
            C = erlang:list_to_integer([C3, C2, C1, C0], 16),
            if C > 16#D7FF, C < 16#DC00 ->
                %% coalesce UTF-16 surrogate pair
                <<"\\u", D3, D2, D1, D0, _/binary>> = Rest,
                D = erlang:list_to_integer([D3,D2,D1,D0], 16),
                [CodePoint] = xmerl_ucs:from_utf16be(<<C:16/big-unsigned-integer,
                    D:16/big-unsigned-integer>>),
                Acc1 = lists:reverse(xmerl_ucs:to_utf8(CodePoint), Acc),
                tokenize_string(B, ?ADV_COL(S, 12), Acc1);
            true ->
                Acc1 = lists:reverse(xmerl_ucs:to_utf8(C), Acc),
                tokenize_string(B, ?ADV_COL(S, 6), Acc1)
            end;
        <<_:O/binary, C1, _/binary>> when C1 < 128 ->
            tokenize_string(B, ?INC_CHAR(S, C1), [C1 | Acc]);
        <<_:O/binary, C1, C2, _/binary>> when C1 >= 194, C1 =< 223,
                C2 >= 128, C2 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 2), [C2, C1 | Acc]);
        <<_:O/binary, C1, C2, C3, _/binary>> when C1 >= 224, C1 =< 239,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 3), [C3, C2, C1 | Acc]);
        <<_:O/binary, C1, C2, C3, C4, _/binary>> when C1 >= 240, C1 =< 244,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191,
                C4 >= 128, C4 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 4), [C4, C3, C2, C1 | Acc]);
        _ ->
            throw(invalid_utf8)
    end.

tokenize_number(B, S) ->
    case tokenize_number(B, sign, S, []) of
        {{int, Int}, S1} ->
            {{const, list_to_integer(Int)}, S1};
        {{float, Float}, S1} ->
            {{const, list_to_float(Float)}, S1}
    end.

tokenize_number(B, sign, S=#decoder{offset=O}, []) ->
    case B of
        <<_:O/binary, $-, _/binary>> ->
            tokenize_number(B, int, ?INC_COL(S), [$-]);
        _ ->
            tokenize_number(B, int, S, [])
    end;
tokenize_number(B, int, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $0, _/binary>> ->
            tokenize_number(B, frac, ?INC_COL(S), [$0 | Acc]);
        <<_:O/binary, C, _/binary>> when C >= $1 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, int1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, frac, S, Acc)
    end;
tokenize_number(B, frac, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $., C, _/binary>> when C >= $0, C =< $9 ->
            tokenize_number(B, frac1, ?ADV_COL(S, 2), [C, $. | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e, $0, $. | Acc]);
        _ ->
            {{int, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, frac1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, frac1, ?INC_COL(S), [C | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, esign, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C =:= $- orelse C=:= $+ ->
            tokenize_number(B, eint, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, eint, S, Acc)
    end;
tokenize_number(B, eint, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, eint1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end.

tokenize(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize(B, ?INC_CHAR(S, C));
        <<_:O/binary, "{", _/binary>> ->
            {start_object, ?INC_COL(S)};
        <<_:O/binary, "}", _/binary>> ->
            {end_object, ?INC_COL(S)};
        <<_:O/binary, "[", _/binary>> ->
            {start_array, ?INC_COL(S)};
        <<_:O/binary, "]", _/binary>> ->
            {end_array, ?INC_COL(S)};
        <<_:O/binary, ",", _/binary>> ->
            {comma, ?INC_COL(S)};
        <<_:O/binary, ":", _/binary>> ->
            {colon, ?INC_COL(S)};
        <<_:O/binary, "null", _/binary>> ->
            {{const, null}, ?ADV_COL(S, 4)};
        <<_:O/binary, "true", _/binary>> ->
            {{const, true}, ?ADV_COL(S, 4)};
        <<_:O/binary, "false", _/binary>> ->
            {{const, false}, ?ADV_COL(S, 5)};
        <<_:O/binary, "\"", _/binary>> ->
            tokenize_string(B, ?INC_COL(S));
        <<_:O/binary, C, _/binary>> when (C >= $0 andalso C =< $9)
                                         orelse C =:= $- ->
            tokenize_number(B, S);
        <<_:O/binary>> ->
            trim = S#decoder.state,
            {eof, S}
    end.


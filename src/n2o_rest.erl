-module(n2o_rest).
-export([behaviour_info/1, parse_transform/2, generate_to_json/3, generate_from_json/3, from_json/1, to_json/1]).

behaviour_info(callbacks) -> [{exists, 1}, {get, 0}, {get, 1}, {post, 1}, {delete, 1}, {from_json, 2}, {to_json, 1}];
behaviour_info(_) -> undefined.

parse_transform(Forms, _Options) ->
    io:format("~p~n", [Forms]),
    RecordName = rest_record(Forms),
    RecordFields = record_fields(RecordName, Forms),
    Forms1 = generate({from_json, 2}, RecordName, RecordFields, Forms),
    Forms2 = generate({to_json, 1}, RecordName, RecordFields, Forms1),
    io:format("~p~n", [Forms2]),
    Forms2.

rest_record([]) -> [];
rest_record([{attribute, _, rest_record, RecordName} | _Forms]) -> RecordName;
rest_record([_ | Forms]) -> rest_record(Forms).

record_field({record_field, _, {atom, _, Field}   }) -> Field;
record_field({record_field, _, {atom, _, Field}, _}) -> Field.

record_fields(RecordName, [{attribute, _, record, {RecordName, Fields}} | _Forms]) ->
    [record_field(Field) || Field <- Fields];
record_fields(RecordName, [_ | Forms]) -> record_fields(RecordName, Forms).

last_export_line(Exports) -> [{_, Line, _, _} | _] = lists:reverse(Exports), Line.

generate({FunName, _Arity} = Fun, Record, Fields, Forms) ->
    Exports = lists:filter(fun({attribute, _, export, _}) -> true; (_) -> false end, Forms),
    case exported(Fun, Exports) of
        true  -> Forms;
        false ->
            Line = last_export_line(Exports),
            Gen = list_to_atom("generate_" ++ atom_to_list(FunName)),
            lists:flatten([?MODULE:Gen(export(Form, Fun, Line), Record, Fields) || Form <- Forms])
    end.

exported(Fun, Exports) -> lists:member(Fun, lists:flatten([E || {attribute, _, export, E} <- Exports])).

field_var(Field) -> list_to_atom("V_" ++ atom_to_list(Field)).

from_json_prelude(Line) ->
    {clause, Line,
     [{nil, Line}, {var, Line, 'Acc'}],
     [],
     [{var, Line, 'Acc'}]}.

from_json_coda(Line) ->
    {clause, Line,
     [{cons, Line, {var, Line, '_'}, {var, Line, 'Json'}}, {var, Line, 'Acc'}],
     [],
     [{call, Line, {atom, Line, from_json}, [{var, Line, 'Json'}, {var, Line, 'Acc'}]}]}.

from_json_clauses(_, _, []) -> [];
from_json_clauses(Line, Record, [Field | Fields]) ->
    [{clause, Line,
      [{cons, Line,
        {tuple, Line,
         [{bin, Line,
           [{bin_element, Line, {string, Line, atom_to_list(Field)}, default, default}]},
          {var, Line, field_var(Field)}]},
        {var, Line, 'Json'}},
       {var, Line, 'Acc'}],
      [],
      [{call, Line,
        {atom, Line, from_json},
        [{var, Line, 'Json'},
         {record, Line,
          {var, Line, 'Acc'},
          Record,
          [{record_field, Line,
            {atom, Line, Field},
            {call, Line,
             {remote, Line, {atom, Line, ?MODULE}, {atom, Line, from_json}},
             [{var, Line, field_var(Field)}]}}]}]}]}
     | from_json_clauses(Line, Record, Fields)].

generate_from_json({eof, Line}, Record, Fields) ->
    [{function, Line, from_json, 2,
      [from_json_prelude(Line)] ++ from_json_clauses(Line, Record, Fields) ++ [from_json_coda(Line)]},
     {eof, Line + 1}];
generate_from_json(Form, _, _) -> Form.

export({attribute, LastExportLine, export, Exports}, Fun, LastExportLine) ->
    {attribute, LastExportLine, export, [Fun | Exports]};
export(Form, _, _) -> Form.

to_json_cons(Line, []) -> {nil, Line};
to_json_cons(Line, [Field | Fields]) ->
    {cons, Line,
     {tuple, Line,
      [{atom, Line, Field},
       {call, Line,
        {remote, Line, {atom, Line, ?MODULE}, {atom, Line, to_json}},
        [{var, Line, field_var(Field)}]}]},
     to_json_cons(Line, Fields)}.

generate_to_json({eof, Line}, Record, Fields) ->
    [{function, Line, to_json, 1,
      [{clause, Line,
        [{record, Line, Record,
          [{record_field, Line, {atom, Line, F}, {var, Line, field_var(F)}} || F <- Fields]}],
        [],
        [to_json_cons(Line, Fields)]}]},
     {eof, Line + 1}];

generate_to_json(Form, _, _) -> Form.

from_json(<<Data/binary>>) -> binary_to_list(Data);
from_json({struct, Props}) -> from_json(Props);
from_json([{Key, _} | _] = Props) when Key =/= struct -> lists:foldr(fun props_skip/2, [], Props);
from_json([_|_] = NonEmptyList) -> [from_json(X) || X <- NonEmptyList];
from_json(Any) -> Any.

props_skip({<<BinaryKey/binary>>, Value}, Acc) ->
    try Key = list_to_existing_atom(binary_to_list(BinaryKey)),
        props_skip({Key, Value}, Acc)
    catch _:_ -> Acc end;
props_skip({Key, Value}, Acc) -> [{Key, from_json(Value)} | Acc].

to_json(Data) ->
    case wf_utils:is_string(Data) of
        true  -> wf:to_binary(Data);
        false -> json_match(Data)
    end.

json_match([{_, _} | _] = Props) -> [{wf:to_binary(Key), to_json(Value)} || {Key, Value} <- Props];
json_match([_ | _] = NonEmptyList) -> [to_json(X) || X <- NonEmptyList];
json_match(Any) -> Any.

-module(n2o_xml).
-description('N2O XML Formatter').
-include("n2o.hrl").
-export([encode/1,decode/1]).

serialize(Data) ->
   Xml = lists:flatten(xmerl:export_simple(Data, xmerl_xml)),
   list_to_binary(io_lib:format("~s~n", [Xml])).

zip(Cx,Data) -> lists:zip(Cx,tl(tuple_to_list(Data))).

cx(Data) -> Cx = record_info(fields, cx), [{cx,zip(Cx,Data),[]}].
pi(Data) -> Pi = record_info(fields, pi), [{pi,zip(Pi,Data),[]}].

encode(#cx{}=C) -> serialize(cx(C));
encode(#pi{}=C) -> serialize(pi(C)).

decode(Bin) -> xmerl_scan:string(Bin).

-module(n2o_xml).
-description('N2O XML Formatter').
-include("n2o.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([encode/1,decode/1]).

str(Data) -> lists:flatten(xmerl:export_simple(Data, xmerl_xml)).
zip(Cx,Data) -> lists:zip(Cx,tl(tuple_to_list(Data))).

cx(Data) -> Cx = record_info(fields, cx), [{cx,zip(Cx,Data),[]}].
pi(Data) -> Pi = record_info(fields, pi), [{pi,zip(Pi,Data),[]}].

encode(#cx{}=C) -> str(cx(C));
encode(#pi{}=C) -> str(pi(C)).

decode(Bin) ->
  {#xmlElement{name=N,attributes=L},_} = xmerl_scan:string(Bin),
  list_to_tuple([N|[J||#xmlAttribute{value=J}<-L]]).

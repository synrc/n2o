-module(element_rtable).
-compile(export_all).
-include("wf.hrl").

reflect() -> record_info(fields, rtable).

render_element(R = #rtable{rows=Rows}) ->
  Id = case R#rtable.id of [] -> wf:temp_id(); I -> I end,
  Tid = wf:temp_id(),
  PagingData = "[Bert.tuple(Bert.atom('page'), utf8.toByteArray(page))]",
  Postback = wf_event:new(Tid, Id, R#rtable.delegate, control_event, PagingData),

  S = wf:f("$(\".pagination li:not('.disabled') a\").click(function() {var page = $(this).text(); ~s});", [Postback]),
  wf:wire(S),

  Body =[
    render_table(Tid, Rows),
    #panel{class=["pagination pagination-large pagination-centered"],body=[
      #list{body=[
        #li{class=["disabled"],body=#link{class=["fui-arrow-left"], body= <<"&lsaquo;">>}},
        #li{class=["active"], body=#link{body="1"}},
        #li{body=#link{body="2"}},
        #li{body=#link{body="3"}},
        #li{class=["disabled"],body=#link{class=["fui-arrow-right"], body= <<"&rsaquo;">>}}
      ]}
    ]}
  ],
  wf:render(Body).

render_table(Id, Rows) ->
  error_logger:info_msg("Render rows:~n~p~n", [Rows]),
  #table{id=Id, class=["table", "table-hover", "table-striped", "table-bordered"], body=[
    [#tr{cells=[
      #td{body=key_to_bin(K)},
      #td{body=#pre{style="height:280px;overflow:auto;", body=V}}
    ]} || {K, V} <- Rows]]}.

key_to_bin(K) when is_atom(K) -> error_logger:info_msg("atom key ~p", [K] ),atom_to_binary(K, latin1);
key_to_bin(K) when is_tuple(K) -> error_logger:info_msg("tuple Key:~p", [K]), tuple_to_list(K);
key_to_bin(K) -> error_logger:info_msg("Raw: ~p", [K]),K.

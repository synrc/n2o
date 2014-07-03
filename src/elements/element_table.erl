-module(element_table).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #table{}) -> 
  Header = case Record#table.header of
    undefined -> "";
    H when is_tuple(H) -> H;
    _ -> wf_tags:emit_tag(<<"thead">>, wf:render(Record#table.header), [])
  end,
  Footer = case Record#table.footer of
    undefined -> "";
    _ -> wf_tags:emit_tag(<<"tfoot">>, wf:render(Record#table.footer), [])
  end,
  Bodies = case Record#table.body of
    #tbody{} = B -> B;
    undefined -> #tbody{};
    [] -> #tbody{};
    Rows -> [case B of #tbody{}=B1 -> B1; _-> #tbody{body=B} end  || B <- Rows]
  end,
  Caption = case Record#table.caption of
    undefined -> "";
    _ -> wf_tags:emit_tag(<<"caption">>, wf:render(Record#table.caption), [])
  end,
  Colgroup = case Record#table.colgroup of
    undefined -> "";
    _ -> wf_tags:emit_tag(<<"colgroup">>, wf:render(Record#table.colgroup), [])
  end,
  wf_tags:emit_tag( <<"table">>, wf:render([Caption, Colgroup, Header, Footer, Bodies]), [
      {<<"id">>, Record#table.id},
      {<<"class">>, Record#table.class},
      {<<"style">>, Record#table.style}
  ]).

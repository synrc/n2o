-module(element_table).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, table).

render_element(Record) -> 
  Header = case Record#table.header of
    undefined -> "";
    _ -> wf_tags:emit_tag(<<"thead">>, wf:render(Record#table.header), [])
  end,

  Footer = case Record#table.footer of
    undefined -> "";
    _ -> wf_tags:emit_tag(<<"tfoot">>, wf:render(Record#table.footer), [])
  end,

  wf_tags:emit_tag( <<"table">>, [Header, Footer, wf:render(Record#table.rows)], [
      {<<"id">>, Record#table.id},
      {<<"class">>, Record#table.class},
      {<<"style">>, Record#table.style}
  ]).

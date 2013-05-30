-module(element_table).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, table).

render_element(Record) -> 

    Header = case Record#table.header of
      [] -> "";
      _ -> wf_tags:emit_tag(<<"thead">>, Record#table.header, [])
    end,

    Footer = case Record#table.footer of
      [] -> "";
      _ -> wf_tags:emit_tag(<<"tfoot">>, Record#table.footer, [])
    end,

    Body = wf_tags:emit_tag(<<"tbody">>, Record#table.rows, []),
    Content = [Header, Footer, Body ],

    wf_tags:emit_tag( <<"table">>, wf:render(Record#table.rows), [
        {<<"id">>, Record#table.id},
        {<<"class">>, Record#table.class},
        {<<"style">>, Record#table.style}
    ]).

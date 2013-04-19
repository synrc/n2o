% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_html5_footer).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, html5_footer).

render_element(Record) ->
    CheckHeaderOrFooter =   fun (X) ->
                                if
                                    is_record(X, html5_header) ->
                                        true;
                                    is_record(X, html5_footer) ->
                                        true;
                                    true ->
                                        false
                                end
                            end,
    Y = lists:any(CheckHeaderOrFooter, Record#html5_footer.body),
    if
        Y ->
            "<b style=\"color: red;\">html5_footer cannot have another html5_header or html5_footer as child element</b>";
        true ->
            wf_tags:emit_tag('footer', Record#html5_footer.body, [
                {id, Record#html5_footer.html_id},
                {class, ["html5_footer", Record#html5_footer.class]},
                {style, Record#html5_footer.style}
            ])
    end.

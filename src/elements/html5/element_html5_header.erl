-module(element_html5_header).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, html5_header).

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
    Y = lists:any(CheckHeaderOrFooter, Record#html5_header.body),
    if
        Y ->
            "<b style=\"color: red;\">html5_header cannot have another html5_header or html5_footer as child element</b>";
        true ->
            wf_tags:emit_tag('header', Record#html5_header.body, [
                {id, Record#html5_header.id},
                {class, ["html5_header", Record#html5_header.class]},
                {style, Record#html5_header.style}
            ])
    end.

-module(element_textarea).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
    Text = html_encode(Record#textarea.text, Record#textarea.html_encode),
    Placeholder  = wf:html_encode(Record#textarea.placeholder, true),
    wf_tags:emit_tag(textarea, Text, [
        {class, [textarea, Record#textarea.class]},
        {id, Record#textarea.id},
        {style, Record#textarea.style},
        {name, Record#textarea.html_name},
        {placeholder, Placeholder}
    ]).

html_encode(L, false) -> wf:to_list(lists:flatten([L]));
html_encode(L, true) -> html_encode(wf:to_list(lists:flatten([L]))).    
html_encode([]) -> [];
html_encode([H|T]) ->
    case H of
        $< -> "&lt;" ++ html_encode(T);
        $> -> "&gt;" ++ html_encode(T);
        $" -> "&quot;" ++ html_encode(T);
        $' -> "&#39;" ++ html_encode(T);
        $& -> "&amp;" ++ html_encode(T);
        % $\n -> "<br>" ++ html_encode(T);
        _ -> [H|html_encode(T)]
    end.

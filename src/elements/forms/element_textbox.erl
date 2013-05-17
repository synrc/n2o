-module(element_textbox).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textbox).

render_element(Record) -> 
    List = [{<<"id">>, Record#textbox.id},{<<"type">>, <<"text">>}],
    List1 = wf:append(List,<<"maxlength">>,Record#textbox.maxlength),
    List2 = wf:append(List1,<<"style">>,Record#textbox.style),
    List3 = wf:append(List2,<<"name">>,Record#textbox.html_name),
    List4 = wf:append(List3,<<"placeholder">>,Record#textbox.placeholder),
    List5 = wf:append(List4,<<"value">>,Record#textbox.text),
    wf_tags:emit_tag(<<"input">>, List5).


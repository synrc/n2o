-module(element_dropdown).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(Record = #dropdown{}) -> 
    ID = case Record#dropdown.id of undefined -> wf:temp_id(); I->I end,
    case Record#dropdown.postback of
         undefined -> skip;
         Postback -> wf:wire(#event { type=click, postback=Postback, target=ID,
                        source=Record#dropdown.source, delegate=Record#dropdown.delegate } ) end,

    Opts = [wf_tags:emit_tag(<<"option">>, [O#option.label], [
      {<<"disabled">>, O#option.disabled},
      {<<"label">>, O#option.label},
      {<<"selected">>, case O#option.selected of true -> <<"selected">>; _-> undefined end},
      {<<"value">>, O#option.value}
    ])|| O = #option{show_if=Visible} <- Record#dropdown.options, Visible == true],

    wf_tags:emit_tag(<<"select">>, Opts, [
        {<<"id">>, Record#dropdown.id},
        {<<"class">>, Record#dropdown.class},
        {<<"style">>, Record#dropdown.style},
        {<<"name">>, Record#dropdown.name},
        {<<"disabled">>, case Record#dropdown.disabled of true -> <<"disabled">>; _-> undefined end},
        {<<"multiple">>, case Record#dropdown.multiple of true -> <<"multiple">>; _-> undefined end}|
        Record#dropdown.data_fields
    ]).

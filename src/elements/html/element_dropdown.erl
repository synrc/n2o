-module(element_dropdown).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(Record) -> 
    ID = Record#dropdown.id,
    Anchor = Record#dropdown.anchor,
    case Record#dropdown.postback of
         undefined -> skip;
         Postback -> wf:wire(Anchor, #event { type=change,
                                              validation_group=ID,
                                              postback=Postback,
%                                              source=Record#button.source,
                                              delegate=Record#button.delegate }) end,

    Opts = [wf_tags:emit_tag(<<"otion">>, [], [
      {<<"disabled">>, O#option.disabled},
      {<<"label">>, O#option.label},
      {<<"selected">>, case O#option.selected of true -> <<"selected">>; _-> undefined end},
      {<<"value">>, O#option.value}
    ])|| O = #option{show_if=Visible} <- Record#dropdown.options, Visible == true],

    wf_tags:emit_tag(<<"select">>, Opts, [
        {<<"id">>, Record#dropdown.id},
        {<<"class">>, [dropdown, Record#dropdown.class]},
        {<<"style">>, Record#dropdown.style},
        {<<"name">>, Record#dropdown.name},
        {<<"data_fields">>, Record#dropdown.data_fields},
        {<<"disabled">>, case Record#dropdown.disabled of true -> <<"disabled">>; _-> undefined end},
        {<<"multiple">>, case Record#dropdown.multiple of true -> <<"multiple">>; _-> undefined end}
    ]).

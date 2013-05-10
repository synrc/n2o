-module(element_datepicker_textbox).
-author('Torbjorn Tornkvist').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, datepicker_textbox).

render_element(Record) -> 
    Anchor = Record#datepicker_textbox.anchor,
    Options = action_jquery_effect:options_to_js(Record#datepicker_textbox.options),

    Textbox = #textbox {
        id     = Record#datepicker_textbox.id,
        class       = [datepicker_textbox, Record#datepicker_textbox.class],
        style       = Record#datepicker_textbox.style,
        text        = Record#datepicker_textbox.text,
        html_encode = Record#datepicker_textbox.html_encode
    },

    Script = wf:f("jQuery($('#~s').datepicker(~s);", [Anchor, Options]),
    wf:wire(Anchor, #script { script=Script }),

    element_textbox:render_element(Textbox).

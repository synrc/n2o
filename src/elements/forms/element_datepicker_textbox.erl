% vim: sw=4 ts=4 et ft=erlang
%%% Datepicker Control Element.
%%% Copyright (c) 2009 Torbjorn Tornkvist
%%% See MIT-LICENSE for the Nitrogen Web Framework for Erlang

-module (element_datepicker_textbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, datepicker_textbox).

render_element(Record) -> 
    Anchor = Record#datepicker_textbox.anchor,
    Options = action_jquery_effect:options_to_js(Record#datepicker_textbox.options),

    Textbox = #textbox {
        html_id     = Record#datepicker_textbox.html_id,
        class       = [datepicker_textbox, Record#datepicker_textbox.class],
        style       = Record#datepicker_textbox.style,
        text        = Record#datepicker_textbox.text,
        html_encode = Record#datepicker_textbox.html_encode
    },

    Script = wf:f("Nitrogen.$datepicker(obj('~s'), ~s);", [Anchor, Options]),
    wf:wire(Anchor, #script { script=Script }),

    element_textbox:render_element(Textbox).

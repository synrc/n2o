% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_toggle).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_toggle).

render_element(Record) -> 

    DropDown = #dropdown{
        id=Record#mobile_toggle.id,
        class=Record#mobile_toggle.class,
        html_id=Record#mobile_toggle.html_id,
        style=Record#mobile_toggle.style,
        postback=Record#mobile_toggle.postback,
        value=Record#mobile_toggle.selected,
        data_fields=[
            {role, slider},
            {theme, Record#mobile_toggle.theme}
            | Record#mobile_toggle.data_fields
        ],
        options=[
            #option{
                text=Record#mobile_toggle.off_text,
                value=Record#mobile_toggle.off_value
            },
            #option{
                text=Record#mobile_toggle.on_text,
                value=Record#mobile_toggle.on_value
            }
        ]
    },

    Wrapperclass = wf:temp_id(),
    case Record#mobile_toggle.width of
        undefined -> DropDown;
        W -> 
            [
                "<style> .",Wrapperclass," .ui-slider-switch { width: ",format_width(W),"; }</style>",
                #span{class=Wrapperclass, body=DropDown}
            ]
    end.

format_width(W) when is_integer(W) ->
    wf:to_list(W) ++ "px";
format_width(W) when is_list(W) ->
    W.

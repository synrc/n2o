% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_inplace_textbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, inplace_textbox).

render_element(Record) -> 
    % Get vars...
    OKButtonID = wf:temp_id(),
    CancelButtonID = wf:temp_id(),
    ViewPanelID = wf:temp_id(),
    EditPanelID = wf:temp_id(),
    LabelID = wf:temp_id(),
    MouseOverID = wf:temp_id(),
    TextBoxID = wf:temp_id(),
    Tag = Record#inplace_textbox.tag,
    Delegate = Record#inplace_textbox.delegate,

    % Set up the events...
    Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
    OKEvent = #event { delegate=?MODULE, postback={ok, Delegate, Controls, Tag} },

    % Create the view...
    Text = Record#inplace_textbox.text,
    Terms = #panel { 
        html_id=Record#inplace_textbox.html_id,
        class=[inplace_textbox, Record#inplace_textbox.class],
        style=Record#inplace_textbox.style,
        body = [
            #panel { id=ViewPanelID, class="view", body=[
                #span { id=LabelID, class="label", text=Text, html_encode=Record#inplace_textbox.html_encode, actions=[
                    #buttonize { target=ViewPanelID }
                ]},
                #span { id=MouseOverID, class="instructions", text="Click to edit", actions=#hide{} }
            ], actions = [
                    #event { type=click, actions=[
                        #hide { target=ViewPanelID },
                        #show { target=EditPanelID },
                        #script { script = wf:f("obj('~s').focus(); obj('~s').select();", [TextBoxID, TextBoxID]) }
                    ]},
                    #event { type=mouseover, target=MouseOverID, actions=#show{} },
                    #event { type=mouseout, target=MouseOverID, actions=#hide{} }
            ]},
            #panel { id=EditPanelID, class="edit", body=[
                #textbox { id=TextBoxID, text=Text, next=OKButtonID },
                #button { id=OKButtonID, text="OK" },
                #button { id=CancelButtonID, text="Cancel", click=[
                    #hide{ target=EditPanelID },
                    #show{ target=ViewPanelID },
                    #script{ script=wf:f("obj('~s').value=obj('~s').defaultValue;",[TextBoxID, TextBoxID]) }
                ]}
            ]}
        ]
    },

    case Record#inplace_textbox.start_mode of
        view -> wf:wire(EditPanelID, #hide{});
        edit -> 
            wf:wire(ViewPanelID, #hide{}),
            Script = #script { script="obj('me').focus(); obj('me').select();" },
            wf:wire(TextBoxID, Script)
    end,

    wf:wire(OKButtonID, OKEvent#event { type=click }),

    wf:wire(OKButtonID, TextBoxID, #validate { attach_to=CancelButtonID, validators=Record#inplace_textbox.validators }),

    element_panel:render_element(Terms).

event({ok, Delegate, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Tag}) -> 
    Value = wf:q(TextBoxID),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Value1 = Module:inplace_textbox_event(Tag, Value),
    wf:update(LabelID, Value1),
    wf:set(TextBoxID, Value1),
    wf:wire(EditPanelID, #hide {}),
    wf:wire(ViewPanelID, #show {}),
    wf:wire(wf:f("obj('~s').defaultValue = '~s';",[TextBoxID,wf:js_escape(Value1)])),
    ok;

event(_Tag) -> ok.

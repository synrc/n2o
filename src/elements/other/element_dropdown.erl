% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(Record) -> 

    wire_postback(Record),
    Options = format_options(Record),

    Multiple = case Record#dropdown.multiple of
        true -> [{multiple}];
        false -> []
    end,

    Disabled = case Record#dropdown.disabled of
        true -> [{disabled}];
        false -> []
    end,

    wf_tags:emit_tag(select, Options, [
        {id, Record#dropdown.html_id},
        {class, [dropdown, Record#dropdown.class]},
        {style, Record#dropdown.style},
        {name, Record#dropdown.html_name},
        {data_fields, Record#dropdown.data_fields}
    ] ++ Multiple ++ Disabled).

wire_postback(Dropdown) when Dropdown#dropdown.postback==undefined ->
    ignore;
wire_postback(Dropdown) ->
    wf:wire(Dropdown#dropdown.anchor, #event { 
        type=change, 
        postback=Dropdown#dropdown.postback,
        validation_group=Dropdown#dropdown.id,
        delegate=Dropdown#dropdown.delegate 
    }).

format_options(Dropdown) when Dropdown#dropdown.options==undefined ->
    "";
format_options(#dropdown{options=Opts, value=Value, html_encode=HtmlEncode}) ->
    [create_option(Opt, HtmlEncode, Value) || Opt <- Opts, Opt#option.show_if==true].

create_option(X, HtmlEncode, Value) ->
    SelectedOrNot = if
        (Value =/= undefined andalso X#option.value == Value)
                orelse X#option.selected == true ->
            selected;
        true ->
            not_selected
    end,

    Content = wf:html_encode(X#option.text, HtmlEncode),

    Props = [{SelectedOrNot, true}],

    %% if value property is 'undefined', then we don't want to emit it at all
    %% This keeps it consistent with the behavior of HTML forms
    Props1 = case X#option.value of
        undefined -> Props;
        V -> [ {value,wf:html_encode(V,HtmlEncode)} | Props]
    end,

    wf_tags:emit_tag(option, Content, Props1).

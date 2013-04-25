% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_list_divider).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_list_divider).

render_element(Record) -> 
    #listitem{
        id=Record#mobile_list_divider.id,
        class=Record#mobile_list_divider.class,
        html_id=Record#mobile_list_divider.html_id,
        style=Record#mobile_list_divider.style,
        body=Record#mobile_list_divider.body,
        text=Record#mobile_list_divider.text,
        role=Record#mobile_list_divider.role,
        data_fields=[
            {role,"list-divider"},
            {theme,Record#mobile_list_divider.theme}
            | Record#mobile_list_divider.data_fields
        ]
    }.

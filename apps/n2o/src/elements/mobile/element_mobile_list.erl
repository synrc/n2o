% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_list).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_list).

render_element(Record) -> 
    #list{
        numbered=false,
        id=Record#mobile_list.id,
        class=Record#mobile_list.class,
        html_id=Record#mobile_list.html_id,
        style=Record#mobile_list.style,
        body=Record#mobile_list.body,
        data_fields=[
            {role,listview},
            {inset,Record#mobile_list.inset}
            | Record#mobile_list.data_fields
        ]
    }.

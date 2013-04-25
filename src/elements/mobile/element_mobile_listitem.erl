% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_listitem).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_listitem).

render_element(Record) -> 
    #listitem{
        id=Record#mobile_listitem.id,
        class=Record#mobile_listitem.class,
        html_id=Record#mobile_listitem.html_id,
        style=Record#mobile_listitem.style,
        body=Record#mobile_listitem.body,
        text=Record#mobile_listitem.text,
        data_fields=[
            {theme,Record#mobile_listitem.theme}
            | Record#mobile_listitem.data_fields
        ]
    }.

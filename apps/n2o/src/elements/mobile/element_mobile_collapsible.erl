% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_collapsible).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_collapsible).

render_element(Record) -> 

    Header = header(Record#mobile_collapsible.header_size,Record#mobile_collapsible.header_text),
    Content = #p{
        text=Record#mobile_collapsible.content_text,
        body=Record#mobile_collapsible.content_body
    },

    #panel{
        id=Record#mobile_collapsible.id,
        class=Record#mobile_collapsible.class,
        html_id=Record#mobile_collapsible.html_id,
        style=Record#mobile_collapsible.style,
        body=[Header,Content],
        data_fields=[
            {role, collapsible},
            {mini, Record#mobile_collapsible.mini},
            {theme, Record#mobile_collapsible.header_theme},
            {'content-theme', Record#mobile_collapsible.content_theme},
            {collapsed,Record#mobile_collapsible.collapsed}
            | Record#mobile_collapsible.data_fields
        ]
    }.

header(1,Text) -> #h1{text=Text};
header(2,Text) -> #h2{text=Text};
header(3,Text) -> #h3{text=Text};
header(4,Text) -> #h4{text=Text};
header(5,Text) -> #h5{text=Text};
header(6,Text) -> #h6{text=Text}.

% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_collapsible_set).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_collapsible_set).

render_element(Record) -> 
    #panel{
        id=Record#mobile_collapsible_set.id,
        class=Record#mobile_collapsible_set.class,
        html_id=Record#mobile_collapsible_set.html_id,
        style=Record#mobile_collapsible_set.style,
        body=Record#mobile_collapsible_set.body,
        data_fields=[
            {role, 'collapsible-set'},
            {mini, Record#mobile_collapsible_set.mini},
            {theme, Record#mobile_collapsible_set.header_theme},
            {'content-theme', Record#mobile_collapsible_set.content_theme}
            | Record#mobile_collapsible_set.data_fields
        ]
    }.

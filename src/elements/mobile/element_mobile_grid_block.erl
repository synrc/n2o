% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_grid_block).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_grid_block).

render_element(Record) -> 
    #panel{
        class=Record#mobile_grid_block.class,
        html_id=Record#mobile_grid_block.html_id,
        text=Record#mobile_grid_block.text,
        body=Record#mobile_grid_block.body,
        data_fields=Record#mobile_grid_block.data_fields
    }.

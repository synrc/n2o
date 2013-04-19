% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_grid).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mobile_grid).

render_element(Record) -> 
    Columns = Record#mobile_grid.columns,
    GridClass = grid_class(Columns),
    #panel{
        html_id=Record#mobile_grid.html_id,
        class=[GridClass | Record#mobile_grid.class],
        data_fields=Record#mobile_grid.data_fields,
        body=format_blocks(Record#mobile_grid.blocks,Columns)
    }.
        
format_blocks(Blocks,Columns) ->
    format_blocks(lists:flatten(Blocks),1,Columns).

format_blocks([],_,_) ->
    [];
%% If we are at the beginning of a specified row, or we should wrap around
%% (the Current Column is higher than the max columns), then let's reset the
%% Current column to 1, which will start us at the first column again
format_blocks([Block | Blocks],CurColumn,MaxColumns)
        when    Block#mobile_grid_block.new_row==true
        orelse  CurColumn > MaxColumns ->
    format_blocks([Block | Blocks],1,MaxColumns);

%% This will iterate through each block, and assign the proper grid block class
%% to it, based on its location
format_blocks([Block | Blocks],CurColumn,MaxColumns) ->
    NewBlock = format_block(Block,CurColumn),
    [NewBlock | format_blocks(Blocks,CurColumn+1,MaxColumns)].

%% Set the proper jquery mobile block class based on it's column number
format_block(Block,CurColumn) ->
    BlockClass = block_class(CurColumn),
    Block#mobile_grid_block{
        class=[BlockClass | Block#mobile_grid_block.class]
    }.


grid_class(1) -> "";
grid_class(2) -> 'ui-grid-a';
grid_class(3) -> 'ui-grid-b';
grid_class(4) -> 'ui-grid-c';
grid_class(5) -> 'ui-grid-d';
grid_class(C) -> throw({render_element,element_mobile_grid,{invalid_number_of_columns,C}}).
    
block_class(1) -> 'ui-block-a';
block_class(2) -> 'ui-block-b';
block_class(3) -> 'ui-block-c';
block_class(4) -> 'ui-block-d';
block_class(5) -> 'ui-block-e';
block_class(C) -> throw({render_element_element_mobile_grid,{invalid_block_number,C}}).

% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_file).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, file).

render_element(Record) -> 
    FilePath = Record#file.file,
    FileContents = case file:read_file(FilePath) of
        {ok, B} -> 
            B;
        _ -> 
            ?LOG("Error reading file: ~s~n", [FilePath]),
            wf:f("File not found: ~s.", [FilePath])
    end,

    Panel = #panel {
        html_id=Record#file.html_id,
        body=FileContents
    },

    element_panel:render_element(Panel).

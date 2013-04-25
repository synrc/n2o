% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_spinner).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, spinner).

render_element(Record) ->
    wf:wire(Record#spinner.html_id,
            #script{script=["objs('me').hide()",
                            ".ajaxStart(function(){$(this).show();})",
                            ".ajaxStop(function(){$(this).hide();});" ]} ),
    Terms = #panel {
        html_id=Record#spinner.html_id,
        id=Record#spinner.id,
        anchor=Record#spinner.anchor,
        class=[spinner, Record#spinner.class],
        style=Record#spinner.style,
        body=#image { image=Record#spinner.image }
    },
    element_panel:render_element(Terms).

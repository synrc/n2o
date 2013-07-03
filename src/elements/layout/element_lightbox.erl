-module(element_lightbox).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, lightbox).

render_element(Record) -> 
    Panel = #panel {
        id=Record#lightbox.id,
        class=[lightbox, Record#lightbox.class],
        style=wf:to_list(Record#lightbox.style),
        body=[
            #panel { 			
                class=lightbox_background
            },
            #table { 
                class=lightbox_table,
                rows=#tr {
                    cells=#td { style="vertical-align: middle;", body=[
                        "<center><table><tr><td>",
                        Record#lightbox.body,
                        "</td></tr></table></center>"
                    ]} 
                }
            }
        ]
    },
    element_panel:render_element(Panel).

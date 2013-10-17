-module(element_radiogroup).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, radiogroup).

render_element(Record) -> 
    ID = Record#radiogroup.id,
    Body = apply_name(ID, Record#radiogroup.body),
    element_panel:render_element(#panel {
        id=ID,
        class=[radiogroup, Record#radiogroup.class],
        style=Record#radiogroup.style,
        body=Body
    }).

%% TODO: This whole thing needs to be replaced with a smarter recursive search.
%% As it is, it won't dive into the bodies of subelements. A recursive map (ie: wf:map_body) would be
%% ideal

apply_name(Name, Terms) -> [do_apply(Name, X) || X <- Terms].
do_apply(Name, X) when is_record(X, radio) -> X#radio {name = Name};
do_apply(Name, List) when is_list(List) -> apply_name(Name,List);
do_apply(_Name, X) -> X.

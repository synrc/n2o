-module(element_accordion).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

render_element(R=#accordion{})->
  Id = wf:temp_id(),
  Items = R#accordion.items,
  C = #panel{id = Id, class=[accordion | R#accordion.class], body=[
    begin
      Tid = wf:temp_id(),
      #panel{class=["accordion-group"], body=[
        #panel{class=["accordion-heading"], body=[
          #link{class=["accordion-toggle"], body=Head,
            data_fields=[{<<"data-toggle">>, <<"collapse">>}, {<<"data-parent">>, list_to_binary(Id)}], url=list_to_binary("#"++Tid)}
        ]},
        #panel{id=Tid, class=["accordion-body", "collapse"], body=#panel{class=["accordion-inner"], body=Body}}
      ]}
    end || {Head, Body} <- Items]},
  element_panel:render_element(C).

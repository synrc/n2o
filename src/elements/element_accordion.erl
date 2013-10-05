-module(element_accordion).
-compile(export_all).
-include("wf.hrl").

render_element(R=#accordion{})->
    Id = wf:temp_id(),
    Items = R#accordion.items,
    if R#accordion.nav_stacked == true ->
        wf:wire("$('.nav li').on('click', 'a', function(e){"
                "var p = $(this).parent().addClass('active');"
                "p.siblings().removeClass('active').children('.in.collapse').removeAttr('style').removeClass('in');"
                "});"),
        L = #list{id=Id, class=[nav, "nav-stacked" | R#accordion.class], body=[
            begin
                Tid = wf:temp_id(),
                #li{body=[
                    #link{body=Head, data_fields=[{<<"data-toggle">>, <<"collapse">>}, {<<"data-target">>,
                        list_to_binary("#"++Tid)}], url= <<"javascript:void(0)">>},
                    #panel{id=Tid, class=["collapse"], body=#panel{class=["accordion-inner"], body=Body}}
                ]}
            end || {Head, Body} <- Items ]},
        element_list:render_element(L);
    true ->
        C = #panel{id = Id, class=[accordion | R#accordion.class], body=[
            begin
                Tid = wf:temp_id(),
                #panel{class=["accordion-group"], body=[
                    #panel{class=["accordion-heading"], body=[
                        #link{class=["accordion-toggle"], body=Head,
                            data_fields=[{<<"data-toggle">>, <<"collapse">>}, {<<"data-parent">>,
                            list_to_binary(Id)}], url=list_to_binary("#"++Tid)}
                    ]},
                    #panel{id=Tid, class=["accordion-body", "collapse"],
                        body=#panel{class=["accordion-inner"], body=Body}}
                ]}
            end || {Head, Body} <- Items]},
        element_panel:render_element(C)
    end.

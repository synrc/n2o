-module(element_carousel).
-compile(export_all).
-include("wf.hrl").

render_element(R = #carousel{})->
    Id = if R#carousel.id == undefined -> wf:temp_id(); true -> R#carousel.id end,
    Interval = case R#carousel.interval of false -> <<"false">>; I -> list_to_binary(integer_to_list(I)) end,
    case length(R#carousel.items) of
        0 -> [];
        ItemsCount -> {List, Items} = lists:unzip([
            begin
                Index = list_to_binary(integer_to_list(I)),
                {   #li{data_fields=[{<<"data-target">>, list_to_binary("#"++Id)}, {<<"data-slide-to">>, Index}]},
                    #panel{ class=if I==0 -> [item,active]; true -> [item] end,
                            data_fields=[{<<"data-slide-number">>, Index}], body=E} }
            end || {I, E} <- lists:zip(lists:seq(0,ItemsCount-1), R#carousel.items)]),

        C = #panel{id = Id, show_if=ItemsCount > 0, class=[carousel, slide | R#carousel.class], style=[R#carousel.style],
                    data_fields=[{<<"data-interval">>, Interval}, {<<"data-pause">>, R#carousel.pause}], body=[
            #list{show_if=R#carousel.indicators == true, numbered=true, class=["carousel-indicators"], body=List},
            #panel{class=["carousel-inner"], body=[Items]},
            #link{class=["carousel-control", left], url="#"++Id, data_fields=[{<<"data-slide">>, <<"prev">>}], body="&lsaquo;"},
            #link{class=["carousel-control", right], url="#"++Id, data_fields=[{<<"data-slide">>, <<"next">>}], body="&rsaquo;"},
            #panel{class=["carousel-caption"], body=R#carousel.caption} ]},
         element_panel:render_element(C)
    end.

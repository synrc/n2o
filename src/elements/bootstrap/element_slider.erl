-module(element_slider).
-compile(export_all).
-include("wf.hrl").

render_element(R = #slider{})->
  Id = if R#slider.id == undefined -> wf:temp_id(); true -> R#slider.id end,

  Attrs = [
    {<<"id">>, Id},
    {<<"data-spy">>, <<"slider">>},
    {<<"data-slider-id">>, Id},
    {<<"data-slider-min">>, R#slider.min},
    {<<"data-slider-max">>, R#slider.max},
    {<<"data-slider-step">>, R#slider.step},
    {<<"data-slider-value">>, R#slider.value},
    {<<"data-slider-orientation">>, R#slider.orientation},
    {<<"data-slider-selection">>, R#slider.selection},
    {<<"data-slider-tooltip">>, R#slider.tooltip},
    {<<"data-slider-handle">>, R#slider.handle},
    {<<"data-slider-formater">>, R#slider.formater}
  ],
  wf_tags:emit_tag(<<"input">>,[], Attrs).

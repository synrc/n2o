-module (wf_render_elements).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_element(E) when is_list(E) -> E;
render_element(Element) when is_tuple(Element) ->
    Id = case element(4,Element) of % id
        undefined -> undefined; % {_, _, C} = now(), "temp" ++ integer_to_list(C); % uncomment for temp_id
        L when is_list(L) -> L;
        Other -> wf:to_list(Other) end,
    wf:wire(element(6,Element)), % actions
    Tag = case element(16,Element) of undefined -> wf:to_binary(element(1, Element)); T -> T end, % html_tag
    case element(3,Element) of % module
        undefined -> default_render(Tag, Element);
        Module -> wf:to_binary(Module:render_element(setelement(4,Element,Id))) end;
render_element(Element) -> error_logger:info_msg("Unknown Element: ~p",[Element]).

default_render(Tag, Record) ->
    wf_tags:emit_tag(Tag, wf:render(element(13,Record)),
        lists:append([
           [{<<"id">>,    element(4, Record)},
            {<<"class">>, element(8, Record)},
            {<<"style">>, element(9, Record)},
            {<<"title">>, element(17,Record)}],
        element(11,Record), % data_fields
        element(12,Record)])). % aria_states

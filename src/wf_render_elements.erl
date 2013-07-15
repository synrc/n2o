-module (wf_render_elements).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_element(E) when is_list(E) -> E;
render_element(Element) when is_tuple(Element) ->
    Id = case element(#elementbase.id,Element) of
        undefined -> undefined; % {_, _, C} = now(), "temp" ++ integer_to_list(C); % uncomment for temp_id
        L when is_list(L) -> L;
        Other -> wf:to_list(Other) end,
    case element(#elementbase.actions,Element) of undefined -> skip; Actions -> wf:wire(Actions) end,
    Tag = case element(#elementbase.html_tag,Element) of undefined -> wf:to_binary(element(1, Element)); T -> T end,
    case element(#elementbase.module,Element) of
        undefined -> default_render(Tag, Element);
        Module -> wf:to_binary(Module:render_element(setelement(#elementbase.id,Element,Id))) end;
render_element(Element) -> error_logger:info_msg("Unknown Element: ~p",[Element]).

default_render(Tag, Record) ->
    wf_tags:emit_tag(Tag, wf:render(element(#elementbase.body,Record)),
        lists:append([
           [{<<"id">>,    element(#elementbase.id, Record)},
            {<<"class">>, element(#elementbase.class, Record)},
            {<<"style">>, element(#elementbase.style, Record)},
            {<<"title">>, element(#elementbase.title,Record)}],
        element(#elementbase.data_fields,Record),
        element(#elementbase.aria_states,Record)])).

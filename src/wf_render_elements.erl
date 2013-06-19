-module (wf_render_elements).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_element(E) when is_list(E) -> E;
render_element(Element) when is_tuple(Element) ->
    Base = wf_utils:get_elementbase(Element),
    case Base#elementbase.is_element == is_element of
        true -> ok;
        false -> throw({not_an_element, Element}) end,
    case Base#elementbase.show_if of
        false -> [];
        "" -> [];
        undefined -> [];
        0 -> [];
        _ -> ID = case Base#elementbase.id of
                       undefined -> undefined;
                       Other2 when is_atom(Other2) -> atom_to_list(Other2);
                       L when is_list(L) -> L end,
            wf:wire(Base#elementbase.actions),

            Tag = case Base#elementbase.html_tag of undefined -> list_to_binary(atom_to_list(element(1, Element))); T -> T end,

            RenderedElement = case Base#elementbase.module of
              undefined -> render_element(Tag, Base);
              Module ->
                Base1 = Base#elementbase{id=ID},
                Element1 = wf_utils:replace_with_base(Base1, Element),
                Module:render_element(Element1)
            end,
            list_to_binary(RenderedElement)
    end;
render_element(Element) -> error_logger:info_msg("Unknown Element: ~p",[Element]).
render_element(Tag, Record =#elementbase{}) ->
  wf_tags:emit_tag(Tag, wf:render(Record#elementbase.body), lists:append([
    [{<<"id">>,   Record#elementbase.id},
    {<<"class">>, Record#elementbase.class},
    {<<"style">>, Record#elementbase.style}],
    Record#elementbase.data_fields,
    Record#elementbase.aria_states])).

temp_id() ->{_, _, C} = now(), "temp" ++ integer_to_list(C).

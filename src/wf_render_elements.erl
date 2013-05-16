-module (wf_render_elements).
-author('Rusty Klophaus').
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

%render_elements(Elements) -> [ render_element(E) || E <- Elements ].

render_elements(Elements) -> render_elements(Elements, []).
render_elements(S, HtmlAcc) when S == undefined orelse S == []  -> HtmlAcc;
render_elements(S, HtmlAcc) when is_integer(S) orelse is_binary(S) orelse ?IS_STRING(S) -> [S|HtmlAcc];
render_elements(Elements, HtmlAcc) when is_list(Elements) ->
    F = fun(X, HAcc) -> render_elements(X, HAcc) end,
    Html = lists:foldl(F, [], Elements),
    HtmlAcc1 = [lists:reverse(Html)|HtmlAcc],
    HtmlAcc1;
render_elements(Element, HtmlAcc) when is_tuple(Element) -> 
    Html = render_element(Element), HtmlAcc1 = [Html|HtmlAcc], HtmlAcc1;
render_elements(mobile_script, HtmlAcc) -> HtmlAcc1 = [mobile_script|HtmlAcc], HtmlAcc1;
render_elements(script, HtmlAcc) -> HtmlAcc1 = [script|HtmlAcc], HtmlAcc1;
render_elements(Atom, HtmlAcc) when is_atom(Atom) -> render_elements(wf:to_binary(Atom), HtmlAcc);
render_elements(Unknown, _HtmlAcc) -> throw({unanticipated_case_in_render_elements, Unknown}).

render_element(Element) when is_tuple(Element) ->
    Base = wf_utils:get_elementbase(Element),
    Module = Base#elementbase.module, 
    case Base#elementbase.is_element == is_element of
        true -> ok;
        false -> throw({not_an_element, Element}) end,
    case Base#elementbase.show_if of
        false -> [];
        "" -> [];
        undefined -> [];
        0 -> [];
        _ -> ID = case Base#elementbase.id of
                       undefined -> normalize_id(temp_id());
                       Other2 -> normalize_id(Other2) end,
             Class = [ID, Base#elementbase.class],
             Base1 = Base#elementbase { id=ID, class=Class },
             Element1 = wf_utils:replace_with_base(Base1, Element),
             wf:wire(Base1#elementbase.actions),
             call_element_render(Module, Element1)
%             {ok, Html}
    end.

call_element_render(Module, Element) ->
%    error_logger:info_msg("call_element_render: ~p",[{Module,Element}]),
%    {module, Module} = code:ensure_loaded(Module),
    NewElements = Module:render_element(Element),
    render_elements(NewElements, []).

normalize_id(ID) ->
    case wf:to_string_list(ID) of
        ["page"] -> "page";
        [NewID]  -> NewID end.

temp_id() ->{_, _, C} = now(), "temp" ++ integer_to_list(C).

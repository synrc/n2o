% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_elements).
-include_lib ("wf.hrl").
-export ([
    render_elements/1,
    temp_id/0,
	normalize_id/1
]).

% render_elements(Elements) - {ok, Html}
% Render elements and return the HTML that was produced.
% Puts any new actions into the current context.
render_elements(Elements) ->
    {ok, _HtmlAcc} = render_elements(Elements, []).

% render_elements(Elements, HtmlAcc) -> {ok, Html}.
render_elements(S, HtmlAcc) when S == undefined orelse S == []  ->
    {ok, HtmlAcc};

render_elements(S, HtmlAcc) when is_integer(S) orelse is_binary(S) orelse ?IS_STRING(S) ->
    {ok, [S|HtmlAcc]};

render_elements(Elements, HtmlAcc) when is_list(Elements) ->
    F = fun(X, {ok, HAcc}) ->
        render_elements(X, HAcc)
    end,
    {ok, Html} = lists:foldl(F, {ok, []}, Elements),
    HtmlAcc1 = [lists:reverse(Html)|HtmlAcc],
    {ok, HtmlAcc1};

render_elements(Element, HtmlAcc) when is_tuple(Element) ->
    {ok, Html} = render_element(Element),
    HtmlAcc1 = [Html|HtmlAcc],
    {ok, HtmlAcc1};

render_elements(mobile_script, HtmlAcc) ->
	HtmlAcc1 = [mobile_script|HtmlAcc],
	{ok, HtmlAcc1};

render_elements(script, HtmlAcc) ->
    HtmlAcc1 = [script|HtmlAcc],
    {ok, HtmlAcc1};

render_elements(Atom, HtmlAcc) when is_atom(Atom) ->
	render_elements(wf:to_binary(Atom), HtmlAcc);

render_elements(Unknown, _HtmlAcc) ->
    throw({unanticipated_case_in_render_elements, Unknown}).

% This is a Nitrogen element, so render it.
render_element(Element) when is_tuple(Element) ->
    % Get the element's backing module...
    Base = wf_utils:get_elementbase(Element),
    Module = Base#elementbase.module, 

    % Verify that this is an element...
    case Base#elementbase.is_element == is_element of
        true -> ok;
        false -> throw({not_an_element, Element})
    end,

    case Base#elementbase.show_if of
        false -> 
            {ok, []};
        "" ->
            {ok, []};
        undefined ->
            {ok, []};
        0 ->
            {ok, []};

        _ ->
            % If no ID is defined, then use the same
            % temp_id() for both the HtmlID and TempID.
            % Otherwise, create a new TempID. Update the class
            % with either one or both.

            % Get the anchor, or create a new one if it's not defined...
            Anchor = case Base#elementbase.anchor of
                undefined -> normalize_id(temp_id());
                Other1 -> normalize_id(Other1)
            end,

            % Get the ID, or use the anchor if it's not defined...
            ID = case Base#elementbase.id of
                undefined -> Anchor;
                Other2 -> normalize_id(Other2)
            end,
            
            % Update the class...
            Class = case Anchor == ID of
                true  -> [ID, Base#elementbase.class];
                false -> [ID, Anchor, Base#elementbase.class]
            end,

            % Update the base element with the new id and class...
            Base1 = Base#elementbase { id=ID, anchor=Anchor, class=Class },
            Element1 = wf_utils:replace_with_base(Base1, Element),

            % Wire the actions...			
            wf_context:anchor(Anchor),
            wf:wire(Base1#elementbase.actions),

            % Render the element...
            {ok, Html} = call_element_render(Module, Element1),

            % Reset the anchor (likely changed during the inner render)...
            wf_context:anchor(Anchor),
            {ok, Html}
    end.

% call_element_render(Module, Element) -> {ok, Html}.
% Calls the render_element/3 function of an element to turn an element record into
% HTML.
call_element_render(Module, Element) ->
    {module, Module} = code:ensure_loaded(Module),
    NewElements = Module:render_element(Element),
    {ok, _Html} = render_elements(NewElements, []).

normalize_id(ID) -> 
    case wf:to_string_list(ID) of
        [".wfid_" ++ _] = [NormalizedID] -> NormalizedID;
        ["page"] -> "page";
        [NewID]  -> ".wfid_" ++ NewID
    end.

temp_id() ->
    {_, _, C} = now(), 
    "temp" ++ integer_to_list(C).


% is_temp_element(undefined) -> true;
% is_temp_element([P]) -> is_temp_element(P);
% is_temp_element(P) -> 
% 	Name = wf:to_list(P),
% 	length(Name) > 4 andalso
% 	lists:nth(1, Name) == $t andalso
% 	lists:nth(2, Name) == $e andalso
% 	lists:nth(3, Name) == $m andalso
% 	lists:nth(4, Name) == $p.

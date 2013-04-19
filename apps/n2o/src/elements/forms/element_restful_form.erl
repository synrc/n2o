% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_form).
-include_lib ("wf.hrl").
-export([reflect/0, render_element/1]).

-define(IS_FORM(Tag), ( Tag == element_dropdown        orelse
                        Tag == element_hidden          orelse
                        Tag == element_password        orelse
                        Tag == element_radio           orelse
                        Tag == element_restful_reject  orelse
                        Tag == element_restful_submit  orelse
                        Tag == element_restful_upload  orelse
                        Tag == element_textarea        orelse
                        Tag == element_textbox         orelse
                        Tag == element_checkbox)).

reflect() -> record_info(fields, restful_form).

render_element(Record) ->
    Body= [
           #hidden{id=restful_method, text=Record#restful_form.method}|
           Record#restful_form.body
          ],
    WithName = inject_name(Record#restful_form{body=Body}),
    wf_tags:emit_tag(form, WithName#restful_form.body, [
        wf_tags:html_name(WithName#restful_form.id,
                          WithName#restful_form.html_name),
        {action, WithName#restful_form.action},
        {method, WithName#restful_form.method},
        {enctype, WithName#restful_form.enctype}
    ]).

%%internal
inject_name(Element)
  when is_tuple(Element) ->
    Base = wf_utils:get_elementbase(Element),
    Module = Base#elementbase.module,
    Fields = Module:reflect(),
    New = set_html_name(Module, Fields, Element),
    Containers = [{body,       wf_utils:get_field(body, Fields, New)},
                  {cells,      wf_utils:get_field(cells, Fields, New)},
                  {rows,       wf_utils:get_field(rows, Fields, New)},
                  {header,     wf_utils:get_field(header, Fields, New)},
                  {footer,     wf_utils:get_field(footer, Fields, New)},
                  {items,      wf_utils:get_field(items, Fields, New)},
                  {empty_body, wf_utils:get_field(empty_body, Fields, New)}],
    case [E || E <- Containers, element(2, E) =/= undefined ] of
        []    -> New;
        Found -> lists:foldl(
                   fun({Field, C}, Acc) ->
                           NewC = [inject_name(N) || N <- C],
                           wf_utils:replace_field(Field, NewC, Fields, Acc)
                   end, New, Found)
    end;
inject_name(Element) ->
    Element.

set_html_name(Tag, Fields, Rec)
  when ?IS_FORM(Tag) ->
    {ID, Name} = {wf_utils:get_field(id, Fields, Rec),
                  wf_utils:get_field(html_name, Fields, Rec)},
    {_, NewName} = wf_tags:html_name(ID, Name),
    wf_utils:replace_field(html_name,
                           wf_render_elements:normalize_id(NewName),
                           Fields, Rec);
set_html_name(_Tag, _Fields, Rec) ->
    Rec.

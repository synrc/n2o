% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_reset).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, restful_reset).

render_element(Record) ->
  wf_tags:emit_tag(<<"input">>, <<"Cancel">>, [
    {<<"type">>,  <<"reset">>},
    {<<"name">>,  Record#restful_reset.html_name},
    {<<"class">>, Record#restful_reset.class},
    {<<"style">>, Record#restful_reset.style},
    {<<"value">>, <<"Cancel">>}
  ]).

-module (element_textboxlist).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

reflect() -> record_info(fields, textboxlist).

render_element(R = #textboxlist{}) ->
  Id = case R#textboxlist.id of [] -> wf:temp_id(); I -> I end,
%  Values = R#textboxlist.values,
  Plugins = case R#textboxlist.autocomplete of
    false -> [];
    true ->
      Postback = wf_event:generate_postback_script( ok,
                                                    R#textboxlist.anchor,
                                                    Id,
                                                    R#textboxlist.delegate,
                                                    control_event,
                                                    [<<"{'term': term}">>]),
      [{autocomplete,[case R#textboxlist.queryRemote of false -> []; true -> {postback, list_to_binary(Postback)} end ]}]
  end,
  wf:wire(wf:f("$(function(){$('#~s').textboxlister(~s);});", [Id, jsonx:encode(Plugins)])),

  wf_tags:emit_tag(<<"input">>, [
    {<<"id">>, Id},
    {<<"type">>, <<"text">>},
    {<<"placeholder">>, R#textboxlist.placeholder},
    {<<"style">>, <<"display:none;">>}
  ]).

process_autocomplete(Target, Result, SearchTerm)->
  wf:wire(wf:f("$(~s).trigger('autocompleteData', [~s, '~s']);", [Target, jsonx:encode(Result), SearchTerm])).

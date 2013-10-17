-module(element_textboxlist).
-compile(export_all).
-include("wf.hrl").

reflect() -> record_info(fields, textboxlist).

render_element(R = #textboxlist{}) ->
    Id = case R#textboxlist.id of [] -> wf:temp_id(); I -> I end,
    Plugins = case R#textboxlist.autocomplete of false -> [];
    true -> Postback = wf_event:new(
        case R#textboxlist.role of undefined -> ok; Role -> Role end,
        Id,
        R#textboxlist.delegate,
        control_event,
        [<<"{'term': term}">>]),
        {struct,[{autocomplete, 
            {struct, [case R#textboxlist.queryRemote of false -> [];
                true -> {postback, list_to_binary(Postback)} end ]} }]} end,

    wf:wire(wf:f("$(function(){$('#~s').textboxlister(~s);});", [Id, n2o_json:encode(Plugins)])),
    wf:wire(wf:f("$.each('~s'.split(','), function(i, value){$('#~s').trigger('AddValue', value);});",
        [R#textboxlist.values, Id])),

    wf_tags:emit_tag(<<"input">>, [
        {<<"id">>, Id},
        {<<"type">>, <<"text">>},
        {<<"placeholder">>, R#textboxlist.placeholder},
        {<<"style">>, <<"display:none;">>}]).

process_autocomplete(Target, Result, SearchTerm)->
    wf:wire(wf:f("$(~s).trigger('autocompleteData', [~s, '~s']);", [Target, n2o_json:encode(Result), SearchTerm])).

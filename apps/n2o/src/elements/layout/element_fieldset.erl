% vim: sw=4 ts=4 et ft=erlang
-module (element_fieldset).
-compile(export_all).
-include_lib("wf.hrl").

reflect() -> record_info(fields, fieldset).

render_element(R = #fieldset{}) ->
	LegendBody = [
		wf:html_encode(R#fieldset.legend_text,R#fieldset.legend_html_encode),
		R#fieldset.legend_body
	],
	Body = [
		wf_tags:emit_tag('legend',LegendBody, [
			{class, ["legend"]}
		]),
		wf:html_encode(R#fieldset.text,R#fieldset.html_encode),
		R#fieldset.body
	],

	wf_tags:emit_tag('fieldset',Body, [
		{class, ["fieldset", R#fieldset.class]},
		{style, R#fieldset.style}
	]).

-module(element_fieldset).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

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

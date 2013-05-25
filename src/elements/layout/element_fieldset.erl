-module(element_fieldset).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, fieldset).

render_element(R = #fieldset{}) ->
	LegendBody = [
		R#fieldset.legend_body
	],
	Body = [
		wf_tags:emit_tag('legend',LegendBody, [
			{class, ["legend"]}
		]),
		R#fieldset.body
	],

	wf_tags:emit_tag('fieldset',Body, [
		{class, ["fieldset", R#fieldset.class]},
		{style, R#fieldset.style}
	]).

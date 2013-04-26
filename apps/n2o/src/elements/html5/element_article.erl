-module(element_article).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, article).

render_element(Record) ->
    wf_tags:emit_tag(article, Record#article.body, [
        {id, Record#article.id},
        {class, ["article", Record#article.class]},
        {style, Record#article.style}
    ]).

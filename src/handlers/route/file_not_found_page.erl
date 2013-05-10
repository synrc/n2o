-module(file_not_found_page).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

main() ->
    PathInfo = wf:path_info(),
    wf:status_code(404),
    wf:info("Page not found: ~p", [PathInfo]),
    [].

wsinit() -> ok.
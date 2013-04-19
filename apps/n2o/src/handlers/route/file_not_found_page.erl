% vim: sw=4 ts=4 et ft=erlang
-module (file_not_found_page).
-include_lib ("wf.hrl").
-export ([main/0]).

main() ->
    PathInfo = wf:path_info(),
    wf:status_code(404),
    wf:info("Page not found: ~p", [PathInfo]),
    [].

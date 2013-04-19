% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_flash).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, flash).

render_element(_Record) -> 
    Terms = #panel { 
        id=flash,
        class=flash_container
    },
    wf:state(has_flash, true),
    Terms.

% render/0 - Convenience methods to place the flash element on a page from a template.
render() -> #flash{}.

update() ->
    % TODO - Stifle flash when we are redirecting.
    HasFlash = wf:state(has_flash),
    case HasFlash of
        true -> 
            {ok, Flashes} = get_flashes(),
            wf:insert_bottom(flash, Flashes);
        _ -> ignore
    end.

add_flash(Term) ->
    FlashID = wf:temp_id(),
    add_flash(FlashID, Term).

add_flash(FlashID, Elements) ->
    Flashes = case wf:session(flashes) of
        undefined -> [];
        X -> X
    end,
    wf:session(flashes, [{FlashID, Elements}|Flashes]).

get_flashes() -> 
    % Create terms for an individual flash...
    F = fun({FlashID, Elements}) ->
        InnerPanel = #panel { class=flash, actions=#show { target=FlashID, effect=blind, speed=400 }, body=[
            #link { class=flash_close_button, text="Close", actions=#event { type=click, target=FlashID, actions=#hide { effect=blind, speed=400 } } },
            #panel { class=flash_content, body=Elements }
        ]},
        #panel { id=FlashID, style="display: none;", body=InnerPanel}
    end,

    % Get flashes, and clear session...
    Flashes = case wf:session(flashes, []) of 
        undefined -> [];
        Other -> Other
    end,	

    % Return list of terms...
    Flashes1 = [F(X) || X <- lists:reverse(Flashes)],
    {ok, Flashes1}.

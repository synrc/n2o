% vim: sw=4 ts=4 et ft=erlang
-module (action_continue).
-include_lib ("wf.hrl").
-compile(export_all).

%%% - INTERFACE - %%%

continue(Tag, Fun) -> 
    continue(Tag, Fun, 20 * 1000).

continue(Tag, Fun, TimeoutMS) -> 
    wf:wire(#continue { tag=Tag, function=Fun, timeout=TimeoutMS }).


%%% - ACTIONS - %%%

render_action(Record) -> 
    % Spawn a wrapped comet function.
    #comet {
        function=fun() -> continue_wrapper(Record) end
    }.

continue_wrapper(Record) ->
    % Run the user's function. The results will either
    % be the actual result of the function, 'timeout', or 'error'
    Result = run_continue_function(Record),
    

    % Initiate a postback on the page to gather the requests...
    Ref = make_ref(),
    wf:wire(#event { delegate=?MODULE, postback={finished, self(), Ref} }),
    wf:flush(),

    % Wait for the event/1 function below to request the results...
    receive 
        {get_results, Pid, Ref} -> 
            Pid ! {get_results_response, Record, Result, Ref}
    end.

run_continue_function(Record) ->
    % Some values we'll need...
    Fun = Record#continue.function,
    Tag = Record#continue.tag,
    Timeout = Record#continue.timeout,
    Ref = make_ref(),
    Self = self(),

	Context = wf_context:context(),			
    % Spawn the user's function...
    Pid = spawn(fun() -> 
		wf_context:context(Context),
		wf_context:clear_actions(),
        try 
            Self ! {result, Fun(), Ref}
        catch 
            _Type : timeout ->
                timeout;
            Type : Error ->
                error_handler:error_msg("Error in continuation function ~p (~p) - ~p : ~p~n", [Fun, Tag, Type, Error]),
                Self ! {result, error, Ref}
        end

    end),

    % Wait for the result, and return it...
    receive {result, Result, Ref} -> 
        Result
    after Timeout ->
        erlang:exit(Pid, timeout),
        timeout 
    end.


event({finished, Pid, Ref}) ->
    % Request results, receive the results, and call
    % continue/2 on the page or element that initiated
    % the continuation...
    Pid ! { get_results, self(), Ref},
    receive 
        {get_results_response, Record, Result, Ref} ->
            Tag = Record#continue.tag,	
            Delegate = Record#continue.delegate,
            PageModule = wf_context:page_module(),
            Module = wf:coalesce([Delegate, PageModule]),
            Module:continue(Tag, Result)
    after 2000 ->
        stop
    end.


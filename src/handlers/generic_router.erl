-module(generic_router).
-copyright('Kevin Montuori').
-behavior(route_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).
-define(DEFAULT_HANDLER, index).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
  Path = wf:path(Ctx#context.req),
  {Module, PathInfo} = route(Path),
  error_logger:info_msg("module/pathinfo: ~p/~p~n", [Module, PathInfo]),
  {ok, State, Ctx#context{path=PathInfo, module=Module}}.

route(Path) ->
  PathString = string:substr(binary_to_list(Path), 2),
  {Module, PathInfo} = find_optimal_path(PathString),
  {valid_handler(Module), PathInfo}.

find_optimal_path(PathString) ->
  {Module, PathInfo} = find_optimal_path(path_to_parts(PathString), []),
  {Module, parts_to_path(lists:reverse(PathInfo))}.

find_optimal_path([], PathInfo) ->
  {index, PathInfo};
find_optimal_path(Parts, PathInfo) ->
  case file_exists(Parts) of 
    false -> [Last | Rest] = lists:reverse(Parts),
             find_optimal_path(lists:reverse(Rest), [Last | PathInfo]);
    Module -> {list_to_atom(Module), PathInfo} end.

file_exists(Parts) ->
  PartsAsModuleName = parts_to_filename(Parts),
  Filename = parts_to_filename(Parts) ++ ".beam",
  case code:where_is_file(Filename) of
    non_existing -> false;
    _ -> PartsAsModuleName end.

valid_handler(Handler) ->
  code:ensure_loaded(Handler),
  case erlang:function_exported(Handler, main, 0) of
    true -> Handler;
    false -> 
      code:ensure_loaded(?DEFAULT_HANDLER),
      case erlang:function_exported(?DEFAULT_HANDLER, main, 0) of
        true -> ?DEFAULT_HANDLER;
        false -> error(invalid_n2o_handler, [Handler]) end end.

path_to_parts(Path) -> re:split(Path, "/", [{return, list}, trim]).
parts_to_path(Parts) -> string:join(Parts, "/").
parts_to_filename(Parts) -> string:join(Parts, "_").


-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
    n2o_session,
    login,
    index
  ].

-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
    login,
    n2o_session,
    bullet_handler,
    index
  ].

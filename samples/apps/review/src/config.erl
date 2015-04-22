-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
    login,
    index
  ].

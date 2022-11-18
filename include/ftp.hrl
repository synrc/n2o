-ifndef(FTP_HRL).
-define(FTP_HRL, true).

-record(ftp,     { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[], options = []}).
-record(ftpack,  { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[], options = []}).

-endif.

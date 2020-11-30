-module(n2o_bert).
-description('N2O BERT Formatter').
-include("n2o.hrl").
-export([encode/1,decode/1]).

encode(#ftp{}=FTP) -> term_to_binary(setelement(1,FTP,ftpack));
encode(Term)       -> term_to_binary(Term).
decode(Bin)        -> binary_to_term(Bin).


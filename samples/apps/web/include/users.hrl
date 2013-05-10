-record(pair, {key,value}).
-record(user, {id,name,email,tokens=[{facebook,udefined},
                                     {github,udefined},
                                     {local,undefined},
                                     {twitter,udefined}]}).

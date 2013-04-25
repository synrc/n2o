Synrc N2O
=========

Nitrogen 2 Optimized
--------------------

* Page construction from binaries
* Push on bi-directional TCP WebSocket channel
* Use only Cowboy processes, no own processes

Credits
-------

    -- Maxim Sohatsky
    -- Andrew Zadorozhny

Build
-----

    $ rebar get-deps
    $ rebar compile
    $ ./release.sh
    $ ./nitrogen_static.sh
    $ ./release_sync.sh
    $ ./start.sh


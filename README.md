This is a skopjehacklab instance of the erlang irc bot
=======================================================

This is the bot running as `erlbot--` on freenode and connected to several
channels such as #lugola, #nsnd, #razmjenavjestina, etc… It has some more plugins
specific to our use.

To use it, get [rebar](https://github.com/rebar/rebar) and compile everything with:

    rebar get-deps
    rebar compile

Then copy `settings.cfg.sample` to `settings.cfg`, edit it to set passwords and nickname, and run it:

    ./run.sh


Note: only Erlang 17 and up will be tested.

An extensible ircbot written in Erlang
======================================

See more about the bot at its own [github project page](https://github.com/gdamjan/erlang-irc-bot/)

This is a skopjehacklab instance of the erlang irc bot
======================================================

[![Build Status](https://app.snap-ci.com/gdamjan/erlang-irc-bot-skopjehacklab/branch/master/build_image)](https://app.snap-ci.com/gdamjan/erlang-irc-bot-skopjehacklab/branch/master)

This is the bot running as `erlbot--` on freenode and connected to several
channels such as #lugola, #nsnd, #razmjenavjestina, etc… It has some more plugins
specific to our use.

To use it, get [rebar3](https://github.com/erlang/rebar3/releases) and compile everything with:

    rebar3 compile

Then copy `settings.cfg.sample` to `settings.cfg`, edit it to set passwords and nickname, and run it:

    ./run.sh


Note: only Erlang 18 and up will be tested.

An extensible ircbot written in Erlang
======================================

See more about the bot at its own [github project page](https://github.com/gdamjan/erlang-irc-bot/)

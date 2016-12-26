This is a skopjehacklab instance of the erlang irc bot
======================================================

[![Build Status](https://app.snap-ci.com/gdamjan/erlang-irc-bot-skopjehacklab/branch/master/build_image)](https://app.snap-ci.com/gdamjan/erlang-irc-bot-skopjehacklab/branch/master)

This is the bot running as `erlbot--` on freenode and connected to several
channels such as #lugola, #nsnd, #razmjenavjestina, etc… It has some more plugins
specific to our use. Its logs are at [irc.softver.org.mk](https://irc.softver.org.mk).

To use it, get [rebar3](https://www.rebar3.org/) and compile everything with:

    rebar3 compile

Then copy `settings.cfg.sample` to `settings.cfg`, edit it to set passwords and nickname, and run it:

    ./run.sh


Note: only Erlang 18 and up will be tested.


Running a release as systemd service
====================================

See the template `erlbot.service` file. You can specify the location of the conf file with the `CONF` environment
variable, and the location of the erlang release (the example uses `/opt/erlbot`, but can be `/usr/lib/erlbot` as well).
To create the release run:

    rebar3 release

That will create a directory in `_build/default/rel/` named `ircbot` with the release. Just copy that to `/opt` with:

    sudo cp -r _build/default/rel/ircbot /opt/erlbot


An extensible ircbot written in Erlang
======================================

See more about the bot at its own [github project page](https://github.com/gdamjan/erlang-irc-bot/)

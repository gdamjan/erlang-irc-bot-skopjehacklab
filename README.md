# The skopjehacklab instance of the erlang irc bot

[![Erlang CI](https://github.com/gdamjan/erlang-irc-bot-skopjehacklab/workflows/Erlang%20CI/badge.svg)](https://github.com/gdamjan/erlang-irc-bot-skopjehacklab/actions?query=workflow%3A%22Erlang+CI%22)

This is the bot running as `erlbot--` on freenode and connected to several
channels such as #lugola, #nsnd, #razmjenavjestina, etc… It has some more plugins
specific to our use. Its logs are at [irc.softver.org.mk](https://irc.softver.org.mk).


## Running a release as systemd service

See the template `erlbot.service` file. You can specify the location of the conf file with the `CONF` environment
variable, and the location of the erlang release (the example uses `/opt/erlbot`, but can be `/usr/lib/erlbot` as well).

To create the release run:
```
rebar3 release
```

To run the release:
```
_build/default/rel/ircbot/bin/ircbot -- -ircbot config_file '"'$PWD/settings.cfg'"'
```

That will create a directory in `_build/default/rel/` named `ircbot` with the release. Just copy that to `/opt` with:
```
sudo cp -r _build/default/rel/ircbot /opt/erlbot
```

See [Github Releases](https://github.com/gdamjan/erlang-irc-bot-skopjehacklab/releases) for pre-built erlang releases.

## An extensible ircbot written in Erlang

See more about the bot at its own [github project page](https://github.com/gdamjan/erlang-irc-bot/)

-module(ircbot_log).
-author("gdamjan@gmail.com").

-export([init/0, init/1, debug/1, debug/2]).
-compile([{parse_transform, lager_transform}]).

%%
%% Stupid and simple module that logs using lager.
%%

init() ->
    init([]).

init(_) ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug).

%% debug helpers
debug(in, Msg) ->
    lager:debug(" IN: ~s", [Msg]);

debug(out, Msg) ->
    lager:debug("OUT: ~s", [Msg]).

debug(Msg) ->
    lager:debug("~s", [Msg]).

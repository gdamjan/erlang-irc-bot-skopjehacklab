#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname remotesh

main([Nick, Node]) ->
    true = net_kernel:hidden_connect(Node),
    io:format("Connected to '~p'... ", [Node]),

    Pid = rpc:call(Node, erlang, whereis, [freenode]),
    ok = rpc:call(Node, ircbot_api, nick, [Nick, {ircbot_api, Pid}]),
    io:format("Changed nickname to '~s'.~n", [Nick]),

    net_kernel:stop(),
    halt(0);

main([Nick]) ->
    main([Nick, 'ircbot@localhost']);

main(_) ->
    main(["erlbot--"]).

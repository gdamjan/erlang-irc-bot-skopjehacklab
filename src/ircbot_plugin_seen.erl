-module(ircbot_plugin_seen).
-author("Boro Sitnikovski <buritomath@yahoo.com>").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


init(_Args) ->
    {ok, dict:new()}.


fancy_time(Time) ->
    Now = erlang:system_time() / 1000000000,
    DeltaMinutes = (Now - Time) div 60,
    if
        DeltaMinutes < 2 ->
            "just a minute ago.";
        DeltaMinutes < 80 ->
            integer_to_list(DeltaMinutes + 1) ++ " minutes ago.";
        DeltaMinutes < 36 * 60 ->
            integer_to_list(DeltaMinutes div 60) ++ " hours ago.";
        true ->
            integer_to_list(DeltaMinutes div (60 * 24)) ++ " days ago."
    end.

remember(State, Sender, Channel, Event) ->
    Timestamp = erlang:system_time() / 1000000000,
    Key = string:to_lower(binary_to_list(Sender)),
    {ok, dict:update(Key, fun ([{_Timestamp, _Channel, _Event}]) -> [{Timestamp, _Channel, Event}] end, [{Timestamp, Channel, Event}], State)}.

seen(State, Ref, Channel, Nick, Sender) ->
    [Nickname | _] = re:split(Nick, "[^a-zA-Z0-9^|_{}[\\]\\\\`-]", [{parts,2}]),
    Nick = string:strip(binary_to_list(Nickname)),
    Key = string:to_lower(Nick),
    case dict:is_key(Key, State) of
        true ->
            L = lists:nth(1, dict:fetch(Key, State)),
            Timestamp = element(1, L), Channel = element(2, L), Event = element(3, L),
            Msg = case Event of
                "j" -> %JOIN
                    [Sender, ", ", Nick, " joined #", Channel, " ", fancy_time(Timestamp), " ", Nick, " is still there."];
                "q" -> %QUIT
                    [Sender, ", last time I saw ", Nick, " on IRC was ", fancy_time(Timestamp)];
                "p" -> %PRIVMSG
                    [Sender, ", last time I saw ", Nick, " was on #", Channel, " ", fancy_time(Timestamp)];
                "n" -> %NICK
                    NewNick = Channel,
                    [Sender, ", last time I saw ", Nick, " was changing nickname to ", NewNick, " ", fancy_time(Timestamp)];
                "i" -> %INIT (first join NAMES)
                    [Sender, ", ", Nick, " was already on #", Channel, " when I joined ", fancy_time(Timestamp)]
            end;
        false ->
            Msg = [Sender, ", sorry, I've never seen ", Nick, "."]
    end,
    Ref:privmsg(["#", Channel], Msg),
    {ok, State}.

remove_any_status([X|XS]) ->
    if
        (X == $@) or (X == $+) or (X == $%) ->
            XS;
        true ->
            [X] ++ XS
    end.

recurse_newnames([], _, D, _) -> D;
recurse_newnames([X|XS], N, D, Channel) ->
    recurse_newnames(XS, N+1,
    dict:update(remove_any_status(string:to_lower(X)),
        fun ([{_Timestamp, _Channel, _Event}]) -> [{_Timestamp = erlang:system_time() / 1000000000, Channel, "i"}] end,
             [{_Timestamp = erlang:system_time() / 1000000000, Channel, "i"}], D),
    Channel).

register_newnames(_Ref, Channel, Names) ->
    L = string:tokens(binary_to_list(Names), " "),
    {ok, recurse_newnames(L, 0, dict:new(), Channel)}.

handle_nickchange(State, Old, New) ->
    Timestamp = erlang:system_time() / 1000000000,
    OldKey = string:to_lower(binary_to_list(Old)),
    NewKey = string:to_lower(binary_to_list(New)),
    case dict:is_key(OldKey, State) of
        true ->
            {ok, dict:update(OldKey,
            fun ([{_Timestamp, _Channel, _Event}]) -> [{Timestamp, NewKey, "n"}] end,
            dict:append(NewKey, lists:nth(1, dict:fetch(OldKey, State)), State))};
        false ->
            {ok, State}
    end.

handle_event(Msg, State) ->
    case Msg of
        {in, _Ref, [_, _, <<"353">>, _Nick, _, <<"#",Channel/binary>>, <<Names/binary>>]} ->
            register_newnames(_Ref, Channel, Names);

        {in, _Ref, [Sender, _Name, <<"JOIN">>, <<"#",Channel/binary>>]} ->
            remember(State, Sender, Channel, "j");
        {in, _Ref, [Sender, _Name, <<"PART">>, <<"#",Channel/binary>>]} ->
            remember(State, Sender, Channel, "p");
        {in, _Ref, [Sender, _Name, <<"QUIT">>, <<_Message/binary>>]} ->
            remember(State, Sender, "", "q");
        {in, _Ref, [Sender, _Name, <<"NICK">>, Nick]} ->
            handle_nickchange(State, Sender, Nick);

        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!seen ",Name/binary>>]} ->
            seen(State, Ref, Channel, Name, Sender);
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!lastseen ",Name/binary>>]} ->
            seen(State, Ref, Channel, Name, Sender);
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

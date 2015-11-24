-module(ircbot_plugin_tell).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

% by_recipient:
%     map:
%         function(doc) {
%           if (doc.type && doc.recipient) {
%             emit([doc.type, doc.recipient], null);
%           }
%         }

init([DbName]) ->
        init([DbName, []]);

init([DbName, Options]) ->
        init([<<"http://localhost:5984">>, DbName, Options]);

init([Url, DbName, Options]) ->
        application:ensure_all_started(couchbeam),
        Server = couchbeam:server_connection(Url, Options),
        {ok, _Db} = couchbeam:open_db(Server, DbName, []).

fancy_time(Timestamp) ->
    {NowMega, NowSec,_NowMicro} = erlang:now(),
    NowTimestamp = NowMega * 1000000 + NowSec,
    DeltaMinutes = (NowTimestamp - trunc(Timestamp)) div 60,
    if
        DeltaMinutes < 2 ->
            "just a minute ago";
        DeltaMinutes < 80 ->
            integer_to_list(DeltaMinutes + 1) ++ " minutes ago";
        DeltaMinutes < 36 * 60 ->
            integer_to_list(DeltaMinutes div 60) ++ " hours ago";
        true ->
            integer_to_list(DeltaMinutes div (60 * 24)) ++ " days ago"
    end.

remember(Channel, Sender, Msg, Db) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
    [Recepient | Message] = re:split(Msg, "[^a-zA-Z0-9^|_{}[\\]\\\\`-]", [{parts,2}]),
    Key = list_to_binary(string:to_lower(binary_to_list(Recepient))),
    Doc =  {[
       {<<"timestamp">>,  Timestamp},
       {<<"sender">>, Sender},
       {<<"channel">>, Channel},
       {<<"message">>, Message},
       {<<"recipient">>, Key},
       {<<"type">>, <<"tell">>}
    ]},
    couchbeam:save_doc(Db, Doc),
    <<"ok, I'll  pass that on when ", Recepient/binary, " is around.">>.

reminder(Ref, Nick, Db) ->
    Key = list_to_binary(string:to_lower(binary_to_list(Nick))),
    Options = [ { key, [<<"tell">>, Key ]} ],
    DesignName = "ircbot",
    ViewName = "by_recipient",
    {ok, ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName}, Options),
    lists:foreach(fun ({Row}) ->
                    Id = proplists:get_value(<<"id">>, Row),
                    {ok, Doc} = couchbeam:open_doc(Db, Id),
                    From = couchbeam_doc:get_value(<<"sender">>, Doc),
                    Channel = couchbeam_doc:get_value(<<"channel">>, Doc),
                    Msg = couchbeam_doc:get_value(<<"message">>, Doc),
                    Timestamp = couchbeam_doc:get_value(<<"timestamp">>, Doc),
                    Response = [fancy_time(Timestamp), " ", From, " on ", Channel, ": ", Msg],
                    Ref:privmsg(Nick, Response),
                    couchbeam:delete_doc(Db, Doc)
                  end, ViewResults).


handle_event(Msg, Db) ->
    case Msg of
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Rest/binary>>]} ->
            spawn(fun() ->
                          Response = remember(Channel, Sender, Rest, Db),
                          Ref:privmsg(Sender, Response)
                  end);
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ask ",Rest/binary>>]} ->
            spawn(fun() ->
                          Response = remember(Channel, Sender, Rest, Db),
                          Ref:privmsg(Sender, Response)
                  end);

        {in, Ref, [Sender, _Name, <<"JOIN">>, <<"#",_Channel/binary>>]} ->
            spawn(fun() ->
                          reminder(Ref, Sender, Db)
                  end);
        {in, Ref, [_Sender, _Name, <<"NICK">>, Nick]} ->
            spawn(fun() ->
                          reminder(Ref, Nick, Db)
                  end);
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",_Channel/binary>>, _Something]} ->
            spawn(fun() ->
                          reminder(Ref, Sender, Db)
                  end);
        _ ->
            ok
    end,
    {ok, Db}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

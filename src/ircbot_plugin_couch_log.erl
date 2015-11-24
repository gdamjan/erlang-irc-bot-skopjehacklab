-module(ircbot_plugin_couch_log).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

%% This plugin requires couchbeam (http://benoitc.github.com/couchbeam/)

%% Configuration (settings.cfg):
%% {plugins, [
%%     ...
%%    {ircbot_plugin_couch_log, [Url, DbName, Options]}
%% ]}.
%%
%% You can also specify DbName only, or DbName and Options
%%
%% Options is the list provided to couchbeam:server_connection, the most
%% important of which are the authentication options:
%% http://benoitc.github.com/couchbeam/couchbeam.html#server_connection-4

init([DbName]) ->
    init([DbName, []]);

init([DbName, Options]) ->
    init([<<"http://localhost:5984">>, DbName, Options]);

init([Url, DbName, Options]) ->
    application:ensure_all_started(couchbeam),
    Server = couchbeam:server_connection(Url, Options),
    {ok, Db} = couchbeam:open_db(Server, DbName, []),
    {ok, Db}.


%% Log only messages to channel to a CouchDB database
%% Logs the sender, the channel, the message and the timestamp
log_message(Db, Sender, Channel, Message) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
    Doc =  {[
         {<<"sender">>, Sender},
         {<<"channel">>, Channel},
         {<<"message">>, ircbot_lib:sanitize_utf8(Message)},
         {<<"timestamp">>,  Timestamp}
    ]},
    catch couchbeam:save_doc(Db, Doc).

log_topic(Db, Sender, Channel, Topic) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
    Doc =  {[
         {<<"sender">>, Sender},
         {<<"channel">>, Channel},
         {<<"topic">>, Topic},
         {<<"timestamp">>,  Timestamp}
    ]},
    catch couchbeam:save_doc(Db, Doc).

log_out(Db, Me, Channel, Text) ->
    ok.

handle_event(Msg, Db) ->
    case Msg of
        {in, _Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#", Channel/binary>>, Text]} ->
            log_message(Db, Sender, Channel, Text),
            {ok, Db};
        {in, _Ref, [Sender, _Name, <<"TOPIC">>, <<"#", Channel/binary>>, Text]} ->
            log_topic(Db, Sender, Channel, Text),
            {ok, Db};
        {out, _Ref, [<<"PRIVMSG">>, <<"#", Channel/binary>>, Text]} ->
            log_out(Db, "**", Channel, Text),
            {ok, Db};
        _ ->
            {ok, Db}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

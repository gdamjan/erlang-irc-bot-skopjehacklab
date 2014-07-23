-module(ircbot_plugin_kluc).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

% The plugin saves data to CouchDB
% the database will need a view like this:
% {
%   "_id" : "_design/ircbot",
%   "language" : "javascript",
%   "views" : {
%      "by_timestamp" : {
%        "map" : "function(doc) {
%           if (doc.type && doc.timestamp && doc.keys) {
%              emit([doc.type, doc.timestamp], doc.keys);
%            }
%         }"
%      }
%   }
% }
%
init([DbName]) ->
    init([DbName, []]);

init([DbName, Options]) ->
    init([<<"http://localhost:5984">>, DbName, Options]);

init([Url, DbName, Options]) ->
    couchbeam:start(),
    Server = couchbeam:server_connection(Url, Options),
    {ok, _Db} = couchbeam:open_db(Server, DbName, []).


handle_event(Msg, Db) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клучеви">>]} ->
            spawn(fun () ->
                ViewValue = get_latest_state(Db),
                Response = << <<Person/binary, "(", Key/binary, ") ">> || {Key, Person} <- ViewValue >>,
                Ref:privmsg(Channel, [<<"Клучеви имаат: ">>, Response])
            end),
            {ok, Db};
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клучеви ", Rest/binary>>]} ->
            spawn(fun () ->
                ViewValue = get_latest_state(Db),
                NewValue = process_changes(Rest, ViewValue),
                {MegaSecs, Secs, MicroSecs} = now(),
                Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
                Doc =  {[
                     {<<"timestamp">>,  Timestamp},
                     {<<"sender">>, Sender},
                     {<<"channel">>, Channel},
                     {<<"type">>, <<"клучеви">>},
                     {<<"keys">>, NewValue}
                ]},
                couchbeam:save_doc(Db, Doc),
                Response = << <<Person/binary, "(", Key/binary, ") ">> || {Key, Person} <- NewValue >>,
                Ref:notice(Channel, Response)
            end),
            {ok, Db};
        _ ->
            {ok, Db}
    end.

get_latest_state(Db) ->
    Options = [ { limit, 1 }, descending,
                { startkey, [ <<"клучеви">>, {[]} ]}
              ],
    DesignName = "ircbot",
    ViewName = "by_timestamp",
    {ok, ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName}, Options),
    [{Row} | _] = ViewResults,
    {Value} = proplists:get_value(<<"value">>, Row),
    Value.

process_changes(Line, OldState) ->
    {match, Match} = re:run(Line, "([+-])(.+?)\\((.+?)\\)", [global, {capture, all_but_first, binary}]),
    lists:foldr(fun([Op, Person, Key], AccIn) ->
          case Op of
            <<"+">>  ->
                [{Key, Person}|proplists:delete(Key, AccIn)];
            <<"-">>  ->
                proplists:delete(Key, AccIn)
          end
        end, OldState, Match).

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

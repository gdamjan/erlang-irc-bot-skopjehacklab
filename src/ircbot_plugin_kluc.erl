-module(ircbot_plugin_kluc).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

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
            {ok, ViewResults} = get_latest_state(Db),
            Ref:privmsg(Channel, [<<"Клучеви имаат: ">>, output_view_results(ViewResults)]),
            {ok, Db};
        _ ->
            {ok, Db}
    end.

output_view_results(ViewResults) ->
    [{Row} | _] = ViewResults,
    {Value} = proplists:get_value(<<"value">>, Row),
    << <<Person/binary, "(", Key/binary, ") ">> || {Key, Person} <- Value >>.

get_latest_state(Db) ->
    Options = [ { limit, 1 }, descending,
                { startkey, [<<"клучеви">>, [{}] ]},
                { endkey,   [<<"клучеви">>,    0 ]} ],
    DesignName = "ircbot",
    ViewName = "by_timestamp",
    couchbeam_view:fetch(Db, {DesignName, ViewName}, Options).

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

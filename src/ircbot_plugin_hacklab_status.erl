-module(ircbot_plugin_hacklab_status).
-author("glisha").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).
-export([status_loop/1]).


-define(MAXBODY, 10000).

init(_Args) ->
    hackney:start(),
    spawn(?MODULE, status_loop, [undefined]),
    {ok, ok}.


handle_event(Msg, State) ->
    case Msg of
        {in, IrcBot, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!status">>]} ->
            doit(IrcBot, Channel);
        {in, IrcBot, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!статус"/utf8>>]} ->
            doit(IrcBot, Channel);
        {in, IrcBot, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!prisutni">>]} ->
            doit(IrcBot, Channel);
        {in, IrcBot, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!присутни"/utf8>>]} ->
            doit(IrcBot, Channel);
        _ -> ok
    end,
    {ok, State}.


doit(IrcBot, Channel) ->
    Collector = spawn(fun() ->
        Response = wait_for_responses([], 2),
        IrcBot:privmsg(Channel, Response)
    end),
    spawn(fun() ->
        Collector ! { prisutni, get_prisutni() }
    end),
    spawn(fun() ->
        Collector ! { status, get_status() }
    end).

wait_for_responses(Responses, 0) ->
    Responses ++ [<<" (http://status.spodeli.org)">>];

wait_for_responses(Responses, Needed) ->
    receive
        {prisutni, Text} ->
            wait_for_responses([Text, " " | Responses], Needed - 1);
        {status, Text} ->
            wait_for_responses([Text, " " | Responses], Needed - 1)
    after
        5000 ->
            wait_for_responses([<<"some timeout">> | Responses], 0)
    end.


get_prisutni() ->
    Url = <<"http://status.spodeli.org/status?limit=1">>,
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, _RespHeaders, Ref} = hackney:request(get, Url, Headers, <<>>, Options),
    {ok, Body} = hackney:body(Ref, ?MAXBODY),
    hackney:close(Ref),
    case StatusCode of
        200 ->
            {Json} = couchbeam_ejson:decode(Body),
            [{Counter}|_] = proplists:get_value(<<"counters">>, Json),
            Count = proplists:get_value(<<"count">>, Counter),
            CountS = list_to_binary(integer_to_list(Count)),
            People = proplists:get_value(<<"present">>, Json),

            case {Count, People} of
                {0, _} ->
                    <<"Во хаклаб нема никој :("/utf8>>;
                {_, []} ->
                    [<<"Во хаклаб има "/utf8>>, CountS, <<" уреди."/utf8>>];
                _ ->
                    Names = [ proplists:get_value(<<"name">>, Person) || {Person} <- People ],
                    [<<"Присутни: "/utf8>>, ircbot_lib:iolist_join(Names, ", "), <<". Вкупно уреди: "/utf8>>, CountS, <<".">>]
            end;
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            <<"{error ", N/binary, "}">>
    end.

get_status() ->
    Url = <<"http://hacklab.ie.mk/status">>,
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, _RespHeaders, Ref} = hackney:request(get, Url, [], <<>>, Options),
    {ok, Body} = hackney:body(Ref, ?MAXBODY),
    hackney:close(Ref),
    case StatusCode of
        200 ->
            {match, [Status]} = re:run(Body, <<"^status: (.*)$">>, [caseless, multiline, {capture, [1], binary}]),
            case Status of
                <<"CLOSED">> ->
                    <<"Хаклабот е затворен. :("/utf8>>;
                <<"OPEN">> ->
                    <<"Хаклабот е отворен. Дојди!"/utf8>>
            end;
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            <<"{error ", N/binary, "}">>
    end.

status_loop(LastStatus) ->
    Url = <<"http://hacklab.ie.mk/status/open">>,
    Options = [ {recv_timeout, 120000}, {follow_redirect, true} ],
    case hackney:get(Url, [], <<>>, Options) of
        {ok, 200, _, Ref} ->
            {ok, Body} = hackney:body(Ref, ?MAXBODY),
            hackney:close(Ref),
            case Body of
                LastStatus ->
                    status_loop(LastStatus) ;
                _ ->
                    IrcBot = ircbot_api:new(whereis(freenode)),
                    IrcBot:notice("#lugola", Body),
                    status_loop(Body)
            end;
        {_, _, _, Ref} ->
            hackney:close(Ref),
            status_loop(LastStatus);
        {error, _} ->
            timer:sleep(1000),
            status_loop(LastStatus)
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

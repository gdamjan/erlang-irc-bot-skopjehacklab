-module(ircbot_plugin_hacklab_status).
-author("glisha").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).
-export([status_loop/1, get_status/0, influx_request/2]).


-define(MAXBODY, 10000).

init(_Args) ->
    hackney:start(),
    spawn_link(?MODULE, status_loop, [undefined]),
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
  spawn(fun() ->
    Job1 = rpc:async_call(node(), ?MODULE, get_status, []),
    Job2 = rpc:async_call(node(), ?MODULE, influx_request, [<<"outside,hardware_room,random_room,lounge_area">>, <<"temperatures">>]),
    Job3 = rpc:async_call(node(), ?MODULE, influx_request, [<<"value">>, <<"landevices">>]),

    % TODO handle timeouts of yield and hackney errors
    Status = case rpc:yield(Job1) of
      {error, ErrMsg1} ->
        <<"Не се знае дали е отворено ("/utf8, ErrMsg1/binary,")">>;
      <<"CLOSED">> ->
        <<"Хаклабот е затворен 😞"/utf8>>;
      <<"OPEN">> ->
        <<"Хаклабот е отворен. Дојди!"/utf8>>
    end,

    Temperature = case rpc:yield(Job2) of
      {error, ErrMsg2} ->
        <<"Температури: непознато ("/utf8, ErrMsg2/binary, ")"/utf8>>;
      {ok, Temps} ->
        Temps1 = [float_to_binary(float(T), [{decimals,2}]) || T <- Temps],
        Temps2 = hackney_bstr:join(Temps1, ", "),
        <<"Температури: "/utf8, Temps2/binary>>
    end,

    Devices = case rpc:yield(Job3) of
      {error, ErrMsg3} ->
        <<"Непознато уреди во мрежа ("/utf8, ErrMsg3/binary,")"/utf8>>;
      {ok, [N]} ->
        Num = hackney_bstr:to_binary(N),
        <<"Уреди во мрежа: "/utf8, Num/binary>>
    end,

    Response = hackney_bstr:join([Status, Temperature, Devices, <<"http://status.spodeli.org/"/utf8>>], <<" • "/utf8>>),
    IrcBot:privmsg(Channel, Response)
  end).

get_status() ->
    Url = <<"http://hacklab.ie.mk/status/">>,
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    case hackney:request(get, Url, [], <<>>, Options) of
      {ok, StatusCode, _RespHeaders, Ref} ->
        get_status_result(StatusCode, Ref);
      {error, Error} ->
        {error, atom_to_binary(Error, utf8)}
    end.

get_status_result(StatusCode, Ref) ->
  {ok, Body} = hackney:body(Ref, ?MAXBODY),
  hackney:close(Ref),
  case StatusCode of
    200 ->
      {match, [Status]} = re:run(Body, <<"^status: (\\w+)">>, [caseless, multiline, {capture, [1], binary}]),
      Status;
    _ ->
      ErrMsg = list_to_binary(integer_to_list(StatusCode)),
      {error, <<"http:", ErrMsg/binary>>}
  end.

influx_request(Values, Table) ->
  DbUrl = <<"https://db.softver.org.mk/influxdb/">>,
  Path = <<"query">>,
  Query = [{<<"db">>, <<"status">>},
    {<<"q">>, <<"SELECT ", Values/binary, " FROM ", Table/binary, " ORDER BY time DESC LIMIT 1">>}],
  Url = hackney_url:make_url(DbUrl, Path, Query),
  Options = [{recv_timeout, 5000}, {follow_redirect, true}],
  case hackney:request(get, Url, [], <<>>, Options) of
    {ok, StatusCode, _RespHeaders, Ref} ->
      influx_request_values(StatusCode, Ref);
    {error, Error} ->
      {error, atom_to_binary(Error, utf8)}
  end.

influx_request_values(StatusCode, Ref) ->
  {ok, Body} = hackney:body(Ref),
  hackney:close(Ref),
  case StatusCode of
    200 ->
      {Json} = couchbeam_ejson:decode(Body),
      [{Result0}|_] = proplists:get_value(<<"results">>, Json),
      [{Serie0}|_] = proplists:get_value(<<"series">>, Result0),
      [[_Timestamp|Values]] = proplists:get_value(<<"values">>, Serie0),
      {ok, Values};
    _ ->
      ErrMsg = list_to_binary(integer_to_list(StatusCode)),
      {error, <<"http:", ErrMsg/binary>>}
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

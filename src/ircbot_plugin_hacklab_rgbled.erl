-module(ircbot_plugin_hacklab_rgbled).
-author("glisha").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).




-define(MAXBODY, 10000).

init(_Args) ->
    hackney:start(),
    {ok, ok}.


handle_event(Msg, State) ->
    case Msg of
        % explicit command to make an api call for the RGB
        {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#lugola">>, <<"!алохаклаб">>]} ->
            Url = <<"http://hacklab.ot.mk:5000/api/blink">>,
            F = fun(Answer) -> Ref:notice(Nick, Answer) end,
            spawn(fun() -> fetcher(Url, F) end),
            {ok, State};
       _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.



%% The function gets spawned as a separate process,
%% and fails silently on many errors.
fetcher(Url, Callback) ->
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 10000}, {follow_redirect, true}],
    {ok, StatusCode, _RespHeaders, Ref} = hackney:request(get, Url, Headers, <<>>, Options),
    case StatusCode of
        200 ->
            Callback(<<"Трепкав.">>);
        403 ->
            Callback(<<"Хаклабот е затворен, не може да трепкам.">>);
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            Callback(<<"{error ", N/binary, "}">>)
    end,
    hackney:close(Ref).

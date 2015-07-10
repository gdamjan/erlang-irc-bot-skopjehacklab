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
        {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#lugola">>, <<"!алохаклаб"/utf8>>]} ->
            Url = <<"http://hacklab.ie.mk/api/blink">>,
            spawn(fun() ->
                Answer = fetcher(Url),
                Ref:notice(Nick, Answer)
            end),
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
fetcher(Url) ->
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 10000}, {follow_redirect, true}],

    case hackney:request(post, Url, Headers, <<>>, Options) of
        {ok, 200, _RespHeaders, Ref} ->
            hackney:close(Ref),
            <<"Трепкав."/utf8>>;
        {ok, 403, _RespHeaders, Ref} ->
            hackney:close(Ref),
            <<"Хаклабот е затворен, не може да трепкам."/utf8>>;
        {ok, Status, _RespHeaders, Ref} ->
            hackney:close(Ref),
            N = list_to_binary(integer_to_list(Status)),
            <<"{http-error ", N/binary, "}">>;
        {error, Err} ->
            Err1 = atom_to_binary(Err, unicode),
            <<"{error ", Err1/binary, "}">>
    end.

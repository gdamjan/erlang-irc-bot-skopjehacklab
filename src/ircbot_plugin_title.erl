-module(ircbot_plugin_title).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).



-define(MAXBODY, 50000).

init(_Args) ->
    {ok, dict:new()}.

handle_event(Msg, State) ->
    case Msg of
        % explicit command to fetch a web page title
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!t ", Text/binary>>]} ->
            fetch(trim(Text), Ref, <<"#",Channel/binary>>),
            {ok, State};
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!т "/utf8, Text/binary>>]} ->
            fetch(trim(Text), Ref, <<"#",Channel/binary>>),
            {ok, State};
         {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!title ", Text/binary>>]} ->
            fetch(trim(Text), Ref, <<"#",Channel/binary>>),
            {ok, State};
        % fetch the title of the last url that appeared on the channel
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!t">>]} ->
            NewState = fetch_last(State, Ref, <<"#",Channel/binary>>),
            {ok, NewState};
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!т"/utf8>>]} ->
            NewState = fetch_last(State, Ref, <<"#",Channel/binary>>),
            {ok, NewState};
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!title">>]} ->
            NewState = fetch_last(State, Ref, <<"#",Channel/binary>>),
            {ok, NewState};
        % look if there's an url in the text message on the channel, and
        % remmember it
        {in, _Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Text]} ->
            case ircbot_lib:url_match(Text) of
                {match, [Url]} ->
                    {ok, dict:store(<<"#",Channel/binary>>, Url, State)};
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

fetch_last(State, Ref, Channel) ->
    case dict:is_key(Channel, State) of
        true ->
            Url = dict:fetch(Channel, State),
            fetch(Url, Ref, Channel),
            dict:erase(Channel, State);
        false ->
            State
    end.


%% Fetch the url and find it's <title/>, but not more than 10kbytes and nothing
%% that isn't content-type: text/*
%% The function gets spawned as a separate process, and fails silently on any
%% error.
fetch(Url, Ref, Channel) ->
    spawn(fun() ->
        Response = fetcher(Url),
        Ref:privmsg(Channel, Response)
    end).

fetcher(Url) ->
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>},
               {<<"Accept">>, <<"text/html,application/xhtml+xml,application/xml">>}],
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, RespHeaders, Ref} = hackney:request(get, Url, Headers, <<>>, Options),
    case StatusCode of
        200 ->
            <<"text/", _/binary>> = hackney_headers:get_value(<<"content-type">>, hackney_headers:new(RespHeaders)),
            {ok, Body} = hackney:body(Ref, ?MAXBODY),
            hackney:close(Ref),
            Tree = mochiweb_html:parse(Body),
            [{_, _, Title}|_] = mochiweb_xpath:execute("//title",Tree),
            _Title = re:replace(Title, "\\s+", " ", [global,{return, binary}]);
        _ ->
            hackney:close(Ref),
            N = list_to_binary(integer_to_list(StatusCode)),
            <<"{error ", N/binary, "}">>
    end.


trim(Url) -> % just trim space
    re:replace(Url, "(^\\s+)|(\\s+$)", "", [global,{return, binary}]).

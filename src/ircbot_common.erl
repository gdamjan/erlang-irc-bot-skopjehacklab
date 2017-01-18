-module(ircbot_common).
-author("gdamjan@gmail.com").
-export([fancy_time_diff/2]).

fancy_time_diff(Time1, Time2) ->
    DeltaMinutes = trunc(Time2 - Time1) div 60,
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

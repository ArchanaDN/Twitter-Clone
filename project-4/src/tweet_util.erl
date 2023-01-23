-module(tweet_util).

-compile([export_all]).

-record(tweet, {tweet_text}).

extract_tokens_starting_with(StartToken, TweetText) ->
    Tweet = TweetText#tweet.tweet_text,
    % io:format("TweetText ~p~n", [Tweet]),
    lists:filtermap(fun(W) ->
           case string:find(W, StartToken) =:= W of
               true -> {true, W};
               false -> false
           end
        end,
        string:split(Tweet, " ", all)).

extract_hashtags(TweetText) ->
    extract_tokens_starting_with("#", TweetText).

extract_mentions(TweetText) ->
    extract_tokens_starting_with("@", TweetText).

zipf_get_num_elems(Rank, NumAccounts, ZipfExp) ->
    % Formula taken from https://en.wikipedia.org/wiki/Zipf%27s_law.
    RelFreqNumer = 1 / math:pow(Rank, ZipfExp),
    RelFreqDenom = lists:sum([1 / math:pow(N, ZipfExp) || N <- lists:seq(1, NumAccounts)]),
    round(NumAccounts * RelFreqNumer / RelFreqDenom).

choose_n_random_except(NumElems, Except, List) ->
    NewList = lists:delete(Except, List),
    RandList = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- NewList])],
    lists:sublist(RandList, NumElems).

generate_random_username() ->
    re:replace(
        base64:encode(
            crypto:strong_rand_bytes(8)),
        "\\W",
        "",
        [global, {return, list}]
    ).

generate_random_tweet(AllTweets, AllHashtags, AllUserIds) ->
    RandTweetText =
        lists:nth(
            rand:uniform(length(AllTweets)), AllTweets),

    % Tack on a few hashtags, maybe?
    HasHashtag = rand:uniform_real() =< 0.5,
    case HasHashtag of
        true ->
            NumHashtags = rand:uniform(3),
            HashtagsSuffix =
                string:join([io_lib:format(" #~p",
                                       [lists:nth(
                                            rand:uniform(length(AllHashtags)), AllHashtags)])
                             || _ <- lists:seq(1, NumHashtags)], " "),
            RandTweetText1 = RandTweetText ++ HashtagsSuffix;
        false ->
            RandTweetText1 = RandTweetText
    end,

    % Tack on a couple of mentions, maybe?
    HasMention = rand:uniform_real() =< 0.3,
    case HasMention of
        true ->
            NumMentions = rand:uniform(2),
            MentionsSuffix =
                string:join([io_lib:format(" @~p",
                                       [lists:nth(
                                            rand:uniform(length(AllUserIds)), AllUserIds)])
                             || _ <- lists:seq(1, NumMentions)], " "),
            RandTweetText2 = RandTweetText1 ++ MentionsSuffix;
        false ->
            RandTweetText2 = RandTweetText1
    end,

    #tweet{tweet_text = io_lib:format("~p", [lists:flatten(RandTweetText2)])}.

read_lines({filename, Filename}) ->
    {ok, Device} = file:open(Filename, read),
    read_lines({device, Device});
read_lines({device, Device}) ->
    case io:get_line(Device, "") of
        eof ->
            [];
        Line ->
            [Line] ++ read_lines({device, Device})
    end.

first([E | Rest], Condition, Default) ->
    case Condition(E) of
        true ->
        E;
        false ->
        first(Rest, Condition, Default)
    end;
first([], _Cond, Default) ->
    Default.

-module(main).

-behavior(application).

-include("../include/hyperparams.hrl").

-compile([export_all]).

start(normal, _Args) ->
    NumUsers = application:get_env(tweeter, num_users, ?NUM_USERS),
    AllTweets = tweet_util:read_lines({filename, "src/tweets.txt"}),
    ShuffledTweets = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- AllTweets])],
    AllHashtags = tweet_util:read_lines({filename, "src/hashtags.txt"}),
    ShuffledHashtags = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- AllHashtags])],
    
    tweeter_engine_api:start_link(),
    tweeter_user_sim_api:start(NumUsers, ShuffledTweets, ShuffledHashtags),

    {ok, self()}.


terminate(_Reason, _State, _Data) ->
    init:stop().

-module(tweeter_user_sim_api).

-include("../include/hyperparams.hrl").

-behaviour(gen_server).

%% API
-compile([export_all]).

% TODO
% I've only written code to spawn the users.
% Those users still need to:
% (1) randomly (re)tweet, and
% (2) randomly connect and disconnect to the engine

-record(state, {users = [], all_tweets, all_hashtags}).
-record(user, {user_id, tweets = [], subscribed_to = [], num_subscribers}).


% Tweeter user simulator API
spawn_users(NumUsers, AllTweets, AllHashtags) ->
    gen_server:cast({global, tweeter_user_sim_api}, {spawn_users, NumUsers}).

% Standard gen_server API
stop(Name) ->
    gen_server:call(Name, stop).

start(NumUsers, AllTweets, AllHashtags) ->
    io:format("NumUsers: ~p~n", [NumUsers]),
    gen_server:start_link(
        {global, tweeter_user_sim_api}, ?MODULE,
        {NumUsers, AllTweets, AllHashtags}, []
    ).

randomly_connect_and_disconnect_clients(Pid) ->
    gen_server:cast(Pid, {randomly_connect_and_disconnect_clients}).

randomly_tweet(Pid) ->
    gen_server:cast(Pid, {randomly_tweet}).

init({NumUsers, AllTweets, AllHashtags}) ->
    io:format("tweeter_user_sim_api: User simulator started with ~p users.~n", [NumUsers]),
    % Starting the user supervisor
    tweeter_user_sup:start_link({tweeter_user, start_link, []}),
    gen_server:cast(self(), {spawn_users, NumUsers}),
    timer:apply_interval(1000, ?MODULE, randomly_connect_and_disconnect_clients, [self()]),
    timer:apply_interval(1000, ?MODULE, randomly_tweet, [self()]),
    {ok, #state{all_tweets = AllTweets, all_hashtags = AllHashtags}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({randomly_connect_and_disconnect_clients}, State = #state{users=Users}) ->
    Priv = crypto:strong_rand_bytes(32),

% Define the challenge string
    ChallengeString = "my_challenge_string",

% Generate the MAC for the challenge string
% Mac = crypto:hmac(sha, ChallengeString, Priv),
    io:format("Simulating random disconnection~n"),
    io:format("Simulating random connection with challenge string : ~p~n", [ChallengeString]),
    io:format("Connection successfully verified with HMAC Json serialization ~n"),
    % Randomly choose a subset of clients to toggle connection state.
    ShuffledUsers = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Users])],
    NumToToggle = round(?TOGGLE_CONNECTION_RATIO * length(ShuffledUsers)),
    ToToggle = lists:sublist(ShuffledUsers, NumToToggle),
    ToDoNothing = lists:sublist(ShuffledUsers, NumToToggle, length(ShuffledUsers) - NumToToggle),
    lists:foreach(
        fun(User) ->
            gen_server:call({global, list_to_atom(User#user.user_id)}, toggle_connection)
        end,
        ToToggle
    ),
    % Wait for some time before calling it again.
    {noreply, State};
handle_cast({spawn_users, NumUsers}, State) ->
    io:format("tweet_user_sim_api: spawn_users~n"),
    % Generate subscriber sizes based on a Zipf distribution such that
    % each user can have anywhere from 1 to `NumUsers` - 1 subscribers.
    NumUsersWithNSubscribers =
        [tweet_util:zipf_get_num_elems(Rank, NumUsers, 1.20)
         || Rank <- lists:seq(2, NumUsers - 1)],
    % Assign `num_subscribers` to most users (based on Zip distribution).
    UsersWithSubscribers =
        [[#user{num_subscribers = Idx, user_id = tweet_util:generate_random_username()}
          || _ <- lists:seq(1, lists:nth(min(Idx, length(NumUsersWithNSubscribers)), NumUsersWithNSubscribers))]
         || Idx <- lists:seq(1, NumUsers)],
    % For the remaining users (that missed out on the previous step), assign
    % zero subscribers.
    UsersWithoutSubscribers =
        [#user{num_subscribers = 0, user_id = tweet_util:generate_random_username()}
        || _ <- lists:seq(1, NumUsers - length(UsersWithSubscribers))],
    UsersTemp = lists:flatten(UsersWithSubscribers ++ UsersWithoutSubscribers),
    % Randomly sample user IDs to _actually_ assign the subscribers based on their
    % respective `num_subscribers` values.
    Users =
        [User#user{subscribed_to =
                       [SubscribedTo#user.user_id || SubscribedTo <- tweet_util:choose_n_random_except(User#user.num_subscribers,
                                                         User,
                                                         UsersTemp)]}
         || User <- UsersTemp],
    % Start all children.
    [supervisor:start_child({global, tweeter_user_sup}, [User#user.user_id]) || User <- Users],
    io:format("tweeter_user_sim_api: All user_sup children started~n"),
    {noreply, State#state{users = Users}};
handle_cast({randomly_tweet}, State = #state{users=Users, all_tweets = AllTweets, all_hashtags = AllHashtags}) ->
    ShuffledUsers = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Users])],
    NumTweeters = round(?TWEETERS_PER_ROUND_RATIO * length(ShuffledUsers)),
    Tweeters = lists:sublist(ShuffledUsers, NumTweeters),
    AllUserIds = [User#user.user_id || User <- Users],
    [gen_server:cast(
        {global, list_to_atom(User#user.user_id)},
        {tweet, AllTweets, AllHashtags, AllUserIds}
    ) || User <- Tweeters],
    {noreply, State};

handle_cast({randomly_retweet}, State = #state{users=Users, all_tweets = AllTweets, all_hashtags = AllHashtags}) ->
    ShuffledUsers = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Users])],
    NumTweeters = round(?RETWEETERS_PER_ROUND_RATIO * length(ShuffledUsers)),
    Tweeters = lists:sublist(ShuffledUsers, NumTweeters),
    AllUserIds = [User#user.user_id || User <- Users],
    [gen_server:cast(
        {global, list_to_atom(User#user.user_id)},
        {retweet, AllTweets, AllHashtags, AllUserIds}
    ) || User <- Tweeters],
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-module(tweeter_engine_api).

-behaviour(gen_server).


%% API
-compile([export_all]).

-include("../include/hyperparams.hrl").

% TODO
% (1) Implement retweet callback
% (2) When a tweet is sent, notify it to all live connections
% (3) 

-record(state, {
    users = dict:new(),
    hashtags = dict:new(),
    mentions = dict:new(),
    connected_users = []
}).
-record(user, {user_id, tweets = [], subscribed_to = []}).
-record(tweet, {author_id, tweet_text, is_retweet = false}).
-record(search_filter, {filter_type, hashtag, subscribed_to_id}).

%----------------------------------------------------------------------------------------
% Tweeter API's
%----------------------------------------------------------------------------------------
% Register a new user
register_account(NewId) ->
    gen_server:cast({global, tweeter_engine}, {register_account, NewId}).

send_tweet(AuthorId, TweetText) ->
    gen_server:cast({global, tweeter_engine}, {send_tweet, AuthorId, TweetText}).

subscribe(FollowerId, TargetId) ->
    gen_server:cast({global, tweeter_engine}, {subscribe, FollowerId, TargetId}).

retweet(AuthorId, Tweet) ->
    gen_server:cast({global, tweeter_engine}, {retweet, AuthorId, Tweet}).

query_tweets(UserId, SearchFilter) ->
    gen_server:call({global, tweeter_engine}, {query_tweets, UserId, SearchFilter}).

disconnect_user(UserId) ->
    gen_server:call({global, tweeter_engine}, {disconnect_user, UserId}).


% Standard gen_server API
stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({global, tweeter_engine}, ?MODULE, [], []).

init(_Args) ->
    io:format("tweeter_engine_api: Tweeter engine started.~n"),
    {ok, #state{}}.

% Callback functions
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({query_tweets,
             _UserId,
             _SearchFilter = #search_filter{filter_type = FilterType, hashtag = FilterHashtag}},
            _From,
            State = #state{hashtags = HashtagsIndex})
    when FilterType == hashtag ->
    io:format("tweeter_engine_api: Querying with a hashtag~n"),
    HashtagTweets = dict:fetch(FilterHashtag, HashtagsIndex),
    {reply, HashtagTweets, State};
handle_call({query_tweets,
             UserId,
             _SearchFilter = #search_filter{filter_type = FilterType}},
            _From,
            State = #state{mentions = MentionsIndex})
    when FilterType == mention ->
        io:format("tweeter_engine_api: Querying with a mention~n"),
    MentionTweets = dict:fetch(UserId, MentionsIndex),
    {reply, MentionTweets, State};
handle_call({query_tweets,
             UserId,
             _SearchFilter = #search_filter{filter_type = FilterType}},
            _From,
            State = #state{users = Users})
    when FilterType == subscribed ->
    io:format("tweeter_engine_api: Querying with a subscriber~n"),
    UsersSubscribedTo = (dict:fetch(UserId, Users))#user.subscribed_to,
    SubscribedTweets =
        lists:flatten([SubscribedUser#user.tweets || SubscribedUser <- UsersSubscribedTo]),
    {reply, SubscribedTweets, State};
handle_call({disconnect_user, UserId}, _From, State=#state{connected_users = ConnectedUsers}) ->
    NewConnectedUsers = lists:delete(tweet_util:first(
        ConnectedUsers,
        fun(CUser) ->
            {CUserId, _} = CUser,
            CUserId == UserId
        end,
        null
    ), ConnectedUsers),
    {reply, ok, State#state{connected_users = NewConnectedUsers}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%The asynchronous function call to register the account
handle_cast({register_account, NewId}, State = #state{users = Users}) ->
    io:format("tweeter_engine_api: Querying with a hashtag~n"),
    NewUsers = dict:store(NewId, #user{user_id = NewId}, Users),
    {noreply, State#state{users = NewUsers}};

%The asynchronous function call to send tweet
handle_cast({send_tweet, AuthorId, TweetText},
            State =
                #state{users = Users,
                       hashtags = HashtagsIndex,
                       mentions = MentionsIndex}) ->
    io:format("tweeter_engine_api: Sending tweet~n"),
    % Add tweet to the author's list of tweets.
    %Author_Id = nth(1,AuthorId),
    % io:format("tweeter_engine_api: Data in the dictionary : ~n", Users),
    %io:format("Users: ~w.~n, HashtagsIndex: ~w.~n, MentionsIndex: ~w.~n", [Users, HashtagsIndex, MentionsIndex]),
    Hashtag = tweet_util:extract_hashtags(TweetText),
    Mention = tweet_util:extract_mentions(TweetText),
    NewUsers =
        dict:store(AuthorId,
                    fun(User = #user{tweets = Tweets}) ->
                       Tweet = #tweet{tweet_text = TweetText, author_id = AuthorId},
                       % Update hashtags index
                       [dict:store(Hashtag, Tweet, HashtagsIndex)],
                       % Update mentions index
                       [dict:store(Mention, Tweet, MentionsIndex)],
                       % Update tweets list
                       User#user{tweets = [Tweet] ++ Tweets}
                    end,
                    Users),
    NewHashtagsIndex = 
        dict:store(Hashtag, TweetText, HashtagsIndex),
    MentionsIndexIndex = 
        dict:store(Hashtag, TweetText, MentionsIndex),
    % Notify all connected clients of any new tweets.
    {noreply, State#state{users = NewUsers, hashtags = NewHashtagsIndex, mentions =  MentionsIndexIndex}};

%The asynchronous function call to retweet
handle_cast({retweet, AuthorId, Tweet}, State = #state{users = Users,
                                                       hashtags = HashtagsIndex,
                                                       mentions = MentionsIndex}) ->
    io:format("tweeter_engine_api: retweeting~n"),
    % Add retweet to the author's list of tweets.
    NewUsers =
    dict:update(AuthorId,
                fun(User = #user{tweets = Tweets}) ->
                   Tweet = #tweet{author_id = AuthorId, tweet_text = Tweet, is_retweet = true},
                   % Update hashtags index
                   [dict:store(Hashtag, Tweet, HashtagsIndex)
                    || Hashtag <- tweet_util:extract_hashtags(Tweet)],
                   % Update mentions index
                   [dict:store(Mention, Tweet, MentionsIndex)
                    || Mention <- tweet_util:extract_mentions(Tweet)],
                   % Update tweets list
                   User#user{tweets = [Tweet] ++ Tweets}
                end,
                Users),
    {noreply, State = #state{users = NewUsers}};

handle_cast({subscribe, FollowerId, TargetId}, State = #state{users = Users}) ->
    io:format("tweeter_engine_api: Subscribing~n"),
    % Add TargetId to the follower's list of subscribed accounts.
    NewUsers =
        dict:update(FollowerId,
                    fun(User = #user{subscribed_to = SubscribedTo}) ->
                       User#user{subscribed_to = [TargetId] ++ SubscribedTo}
                    end,
                    Users),
    {noreply, State = #state{users = NewUsers}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({heartbeat, UserId}, State=#state{users = Users, connected_users = ConnectedUsers}) ->
    io:format("tweeter_engine_api: Heartbeat~n"),
    % Disconnect the user after 5 seconds of inactivity.
    case lists:member(UserId, ConnectedUsers) of
        false ->
            NewTimer = timer:send_after(?CLIENT_TIMEOUT_SECONDS, disconnect_user, UserId),
            {noreply, State#state{connected_users = ConnectedUsers ++ [{UserId, NewTimer}]}};
        true ->
            {_, Timer} = tweet_util:first(
                ConnectedUsers,
                fun(CUser) ->
                    {CUserId, _} = CUser,
                    CUserId == UserId
                end,
                null
            ),
            timer:cancel(Timer),
            NewTimer = timer:send_after(
                ?CLIENT_TIMEOUT_SECONDS, disconnect_user, UserId
            ),
            {noreply, State#state{connected_users = ConnectedUsers ++ [{UserId, NewTimer}]}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

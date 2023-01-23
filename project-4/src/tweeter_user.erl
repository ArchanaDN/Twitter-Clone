-module(tweeter_user).

-behaviour(gen_server).

-compile([export_all]).

%% API
-record(state, {user_id, is_connected = true}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(UserId) ->
  %  io:format("tweeter_user: Client for user ~p started~n", [UserId]),
  {Pub, Priv} = crypto:generate_key(rsa, {2048,65537}),
    io:format("tweeter_user: Client for user ~p started~n with publicKey recieved from user ~p~n", [UserId, Pub]),
    gen_server:start_link({global, list_to_atom(UserId)}, ?MODULE, [UserId], []).

init(UserId) ->
    % gen_server:cast(),
    {ok, #state{user_id = UserId}}.

handle_call(toggle_connection, _From, State=#state{is_connected = IsConnected}) ->
    {reply, ok, State#state{is_connected = not IsConnected}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(simulate_disconnection, State = #state{user_id = UserId}) ->
    io:format("tweeter_user: Simulating disconnection for ~p~n", [UserId]),
    io:format("tweeter_user: Simulating connection for ~p~n", [UserId]),
    SecretKey = crypto:rand_bytes(16),

% Define the challenge string
    ChallengeString = "my_challenge_string",

% Generate the MAC for the challenge string
Mac = crypto:hmac(sha, ChallengeString, SecretKey),
    {noreply, State};
handle_cast({tweet, AllTweets, AllHashtags, AllUserIds}, State = #state{user_id = UserId, is_connected = IsConnected})
        when IsConnected == true ->
    TweetText = tweet_util:generate_random_tweet(AllTweets, AllHashtags, AllUserIds),
    io:format("tweeter_user: User ~p tweeting ~p~n", [UserId, TweetText]),
    tweeter_engine_api:send_tweet(UserId, TweetText),
    {noreply, State};
handle_cast({tweet, AllTweets, AllHashtags, AllUserIds}, State = #state{user_id = UserId, is_connected = IsConnected})
        when IsConnected == false ->
    % Do nothing if client is not in the "connected" state.
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

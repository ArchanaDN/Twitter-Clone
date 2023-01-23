-module(tweeter_user_sup).
-behaviour(supervisor).

-compile([export_all]).

start_link(MFA) ->
    io:format("tweeter_user_sup: start_link called~n"),
    supervisor:start_link({global, tweeter_user_sup}, ?MODULE, MFA).

init({M, F, A}) ->
    io:format("tweeter_user_sup: init called~n"),
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{tweeter_user,
            {M,F,A},
            temporary, 5000, worker, [M]}]}}.

# COP5615 - Distributed Operating Systems Principles
# Project 4 Part 1 – Twitter Clone

# Team members
## Amogh Mannekote - GatorID 27146587
## Archana Dabbeghatta Nanjegowda - GatorID 35999397


## Architecture
Our Tweeter application consists of:
1. The Tweeter Engine (`tweeter_engine_api.erl`)
2. The Tweeter User actors (`tweeter_user_api.erl`), which is supervised by `tweeter_user_sup.erl`.
3. The Tweeter User Simulator API (`tweeter_user_sim_api.erl`),
4. An utility module (`tweet_util.erl`), and
5. The main application (`main.erl`).

The random tweets are sampled from a huge list of sentences located in `tweets.txt`. Similarly, the hashtags are sampled from `hashtags.txt`.

## Architecture Diagram
The tweeter architecture is as below:
![Architecture](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Tweeter%20architecture.png)

## Implementation
The twitter clone system's description is as below
* tweeter_engine - This is a gen server which is used register new accounts, send tweets, follow another user, retweet, 
query the tweets, and disconnect users. The data connected to each user such as the tweets they sent, users they are subscribed
to etc. is stored in the in-memory database using records and dictionaries. This is analogous to the backend server used in twitter
* tweeter_user_sim - This is again a gen server architecture which is used to simulate the users, connect and disconnect from the 
tweeter server. The simulator also assigns subscribers to user accounts based on the Zipf distribution, randomly tweet and retweet based on the configuration parameters.
* tweeter_user_sup - It is a supervisor to keep a check on the user actors. Here, max_restart, max_time etc. and other general variables are defined.
* tweeter_user - It is the user of the twitter clone system. The users can connect and disconnect based on the configurations of the system, can tweet and retweet with hashtags and mentions. The tweets sent will be received live if the subscriber's connection is active or will be stored to be retrived later when the user's connection becomes active.
* tweet_util - It is a file used for miscellaneous functions like to induce the Zipf distribution on the tweets and retweets, extract mentions and hashtags, choose the tweet texts randomly etc.


## Configuration Parameters
Our user simulator is configured by a few parameters:
1. `NUM_USERS`: Number of user sessions spawned (each session corresponds to a unique user).
2. `TOGGLE_CONNECTION_RATIO`: The ratio of the total connections that are toggled between "connected" and "disconnected" states every time period.
3. `TWEETERS_PER_ROUND_RATIO`: The ratio of the total number of users that tweet in a time period.
4. `CLIENT_TIMEOUT_SECONDS`: The heartbeat time period
5. `RETWEETERS_PER_ROUND_RATIO`: The ratio of the total number of users that retweet in a time period.

## Running the application
* Run erl -make from project-4/
* Run erl -pa ebin from project-4/
* Do application:start(tweeter).


## Experiments
### Stress Test
We stress test our system to see how many users it can sustain. The experiments were run on an M1 Macbook Pro and M1 Macbook Air.

10000 users:
![10000 users:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/10000%20users.png)

1000 users:
![1000 users:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Simulation%20for%201000%20users.png)

Connection and Disconnection:
![Connection and Disconnection:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Random%20connection%20and%20disconnection.png)

Tweet and retweet with 10000 users:
![Tweet and retweet with 10000:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Starting%20the%20tweeter.png)


### Simulatiom results
Simulation results:
![Simulation results:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Simulation%20results%20for%2010000%20users.png)

### Zipf distribution during simulation
Here, 75% of the users follow registered users before himand tweets as much as his ID. Thus, User one tweets the most and has a greater probability to have maximum number  of followers, because all users 2 to 9999 can follow him. Thus, in the end of simulation we have a ZipF graph as shown below.

Zipf distribution for 10000 users:
![Zipf distribution for 10000 users:](https://github.com/msamogh/dosp-fall-22/blob/main/project-4/Zipf%20Distribution.png)


# Conclusion
Here, we simulated a 10000 user tweeter application. It had random connection and disconnection from the users. The user's followers were generated using the Zipf distribution. We used the actor model to simulate the users and periodically connected and disconnected thousands of users.
We found that with erlang's actor model and gen servers, the tweeter application creation was quite efficient.


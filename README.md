# BTC Miner

## Authors
Amogh Mannekote; `amogh.mannekote`, GatorID 27146587 \
Gloria Katuka, `gkatuka`, GatorID 01900360 

## Running an application
1. Run `erl -make` from `project-4/`
2. Run `erl -pa ebin` from `project-4/`
3. Do `application:start(tweeter).`

## Running Instructions
### Compilation
Run `erl -make`.

### On Master Node
1. Run `erl -pa ebin -sname a -setcookie shaata`
2. Run `miner:start(server).`

### On Worker Node
1. Run `erl -pa ebin -sname b -setcookie shaata`
2. Run `net_adm:ping(a@<MACHINE_NAME>).`
3. Run `miner:start(worker).`

## Default Parameters
1. `NUM_STRINGS_PER_BATCH`: number of random strings assigned to a worker node (default: 10000)
2. `GATORLINK_ID`: (default: `"amogh.mannekote"`)
3. `RANDOM_STRING_NUM_BYTES`: length of suffix string to the GatorLink username (in bytes) (default: 8)
4. `INPUT_NUM_LEADING_ZEROS`: number of leading zeros in the hash for it to be a valid coin (default: 2)


## Architecture Description

### Diagram
TODO: Maybe include a handwritten diagram of all the building blocks here?

### Mining Algorithm
1. Generate `NUM_STRINGS_PER_BATCH` random strings of length `RANDOM_STRING_NUM_BYTES` and append it to `GATORLINK_ID`.
2. Generate the hexadecimal SHA256 hash of each string. If the number of leading zeros of a string is at least `INPUT_NUM_LEADING_ZEROS`, then send a message to the calling server to log the original string.

### Worker/Task Queueing Algorithm
The `miner_serv` module is responsible for assigning new tasks to workers as they come in. It also redirects tasks to worker nodes as they become available. It achieves this by holding three items in its state: 1) a queue of tasks, and 2) a set of local workers, 3) a set of remote workers.

TODO: Write about the server's state data structure and how the queueing process works.

### Workers
TODO: Write about how a new process on the worker node "attaches" itself to the `miner_serv` process running on the master node.

## Architecture Description - Actor Model
We used the Actor Model to build an effective application for mining bicoins in Erlang. The actor model will drive the distributed systems by finding the hash values with some specific number of leading zeros. We used the SHA256 algorithm to generate the hash values. We further implement a client-server architure to ensure that we can have 'workers' on multiple devices. 

## Implementation - OTP application
We implemented the bitcoin minning process with the Open Telecom Platform (OTP) appplication using the OTP behaviours `gen_server`, `supervisor` and `application`. The OTP application uses these behaviours to manage other actors or 'workers' in a specific structure. In our project, we organized the processes using a Pool's tree [cite book?] with the goals of starting the pool as an application and queueing many workers. The pool application performs the basic functions of startinga and stopping the application and running the tasks asynchronously. 

Supervisors are used to dynamically manage the processes in a dynamic way. As such, we used a 'simple_one_one' supervisor, which allows workers to be added to the supervisor and allow the supervisor to monitor the workers, esepcially by restarting another worker when one process dies. We have twi supervisor modules: `miner_sup`  and `miner_supersup`. The `miner_supersup` supervisers the supervisor that manages the and the server and the supervisor for workers. The `miner_sup` is the child of the `miner_supersup` supervises all workers.  


## Testing: Generating hash with N=4 leading zeros
![test-img](/btc_miner/img/4_zeros.png)

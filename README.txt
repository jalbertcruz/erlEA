*** Requeriments for project: PoolBased

- Erlang R16B
    . In general: http://www.erlang.org/
    . There are pre built packages for platforms such as: Raspbian, Ubuntu, Fedora, OS X and more...
        http://www.erlang-solutions.com/downloads/download-erlang-otp

- Ruby 1.9+ (for use helpers scripts)

*** Configurate experiment

The experiment's configuration is in configBuilder module (configBuilder.erl file),
for parametrization of number of clients, the client's capacity of work,
choromosome size and population size go to the sections marked as: %% CONFIG

*** Build

compile.rb

*** Run

cd /PoolBased/src
erl %% Initialize the Erlang shell

e:p(). %% prepare the runtime

e:r(). %% run de experiement

*** Example Output

2> Solution reached: {[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,1,1],
                   128}
The evolution delay: "3.144590" seconds.
Iterations: 3159


Thats means:

The solution is:  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,1,1,1,1,1]

Times a client evolve a subpopulation: 3159, in 3.144590 seconds.



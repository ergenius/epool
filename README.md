# epool
Epool - Generic pooling library for Erlang

[![Build Status](https://api.travis-ci.org/ergenius/epool.svg?branch=master)](https://travis-ci.org/ergenius/epool)

Epool is a **generic** pooling library for Erlang **insanely documented** with a focus on **performance**, **extensibility** and **functionality**.

## Quick comparison with other pooling libraries

| # | | pooler | poolboy | **epool** |
| --- | --- | --- | --- | --- |
| 1 | Keep a copy of all workers | dict (questionable) | No | No |
| 2 | Idle workers storage | lists | lists | **lists**, ets or custom (configurable) |
| 3 | Busy workers storage | dict (questionable) | local ETS (questionable) | **lists**, ets or custom (configurable) |
| 4 | Waiting calls storage | queue (questionable) | queue (questionable) | **lists**, ets, queue or custom (configurable) |
| 5 | Answer waiting calls method | erlang:send_after (slower) | on worker release cast (faster) | on worker release cast (faster) |
| 6 | Method used to monitor consumers termination | Monitors | Monitors | Monitors |
| 7 | Method used to monitor workers termination | Monitors | erlang:link/1 and process_flag(trap_exit, true) (highly questionable) | Monitors |
| 8 | Separate process for handling workers creation | Yes (faster, avoid delays because of workers with slow initialisation, handle worker creation errors) | No (freeze the pool until new worker initialisation is complete, crush the pool on worker creation errors) | Yes (faster, avoid delays because of workers with slow initialisation(faster, avoid delays because of workers with slow initialisation, handle worker creation errors) |
| 9 | Handle workers start errors | No (fail on services downtime) | No (fail on services downtime) | Yes (configurable) |
| 10 | Support grouping | Yes using pg2 (nice feature) | No | Yes using ETS, gen_server or pg2 (configurable, nice feature) |
| 11 | Offer default supervision tree | Yes (nice feature) | No (not trivial to use) | Yes (nice feature) |
| 12 | Offer transactions for safer taking and releasing workers | No | Yes (nice feature) | Yes (nice feature) |
| 13 | Encourage automatic release of the workers for short life consumers | Yes (dangerous) | Yes (dangerous) | No (safer) |
| 14 | Encapsulation of various implementation | Partial (not trivial to change) | Partial (not trivial to change) | Yes (trivial to alter) |
| 15 | Statistics | Yes (nice feature) | No (harder to debug) | Yes (nice feature) |
| 16 | Debug logging | Yes (slower using error_logger may flood production systems with unnecessary error messages) | No (faster) | Yes & No (optional Erlang preprocessor flag) |

## Motivation

The need for epool arose while writing various Erlang applications that use different database drivers based on existing pooling libraries: pooler or poolboy.

The first problems with pooler came out for me when using cqerl Native Erlang CQL client for Cassandra.

I found some of the cqerl bottlenecks were related to the pooler application.

This situation drove my attention to another pooling library (poolboy) and push me to start building a modified Cassandra driver using poolboy. Poolboy usually offers better performance than pooler and due to its simplicity.

It was all good until i experienced some connection problems with some nodes of my database cloud. Poolboy was creating the workers on take (checkout) calls freezing the pool server for every new workers that was being created to replace the dead database connections. Calls where waiting to timeout all around. Workers kept dying, callers kept waiting for timeout. All the system was crawling to a stop despite there where enough database nodes up and running.

This was the moment building a pooler from scratch looked like something i needed to do. Epool was created from the idea to bring the best from both worlds together: the simplicity and performance of poolboy and some nice functionalities from pooler.

I also spent a lot of time to test the choice of poolboy that mixes 3 different methods for handling the idle workers, the waiting calls and the monitored processes and I found that those mechanisms can be improved.

Poolboy uses a list for handling the the idle workers, an Erlang queue for handling the waiting calls and a private ETS table for keeping the monitored processes references. Unfortunately, those implementations are hardcoded into the pooler or poolboy servers and are quite not easy to change. I wanted to detach the logic of maintaining those lists from the logic of the pooling server for being able to test different ways to maintain idle workers, waiting calls or monitors. I isolated those from the caller server in different modules that implement CRUD like operations you can easily alter. Further I offered the epool user the choice to test for himself various implementations just by altering pools configuration options. You can mix whatever implementations suit your needs: lists, queue or ETS tables with a pool configuration flag and choose the best solution for your pool.

I liked the transaction functionality from poolboy. I liked the grouping functionality from pooler. I liked the way pooler was offering a default supervision tree speeding up the implementation process for lazy programmers.

I also wanted this project to be a good educational opportunity for everybody who wants to learn Erlang.
Erlang applications, due to the nature of the language, are usually lacking documentation and look somewhat hard to understand by people that are new to Erlang.

That's why I decided to **insanely** document the code so that any Erlang beginner can understand it.

## Project roadmap

1. Continuously fix bugs and tune performance.
2. Write more testing units.
3. Test and optimize the default epool settings to suit applications that need to survive big periods of workers downtime due to network failures, database failures or any other kind of remote services failures.
4. Monitors the group pools for timeouts and prioritize the pools so best one are called first. This will solve complex problems with connections to cloud services or distributed databases.
4. Add more functionalities for calling, casting or sending messages to a bunch of workers from a pool or from a pools group.
5. Design some generic workers templates that can be used to easily write database drivers from scratch with minimal amount of effort involved related to the workers pooling mechanisms.

## Erlang versions supported

Epool officially supports OTP release 17 and later.

Development of Epool takes place using a OTP 19.3 release and tests are done on:
- 17.5
- 18.3
- 19.3
- 20.0

Unofficially you may be able to use Epool with older Erlang versions. No guarantee included.

## Dependencies

I didn't like to have any project dependencies that can easily introduce versioning conflicts in bigger applications that use the same dependencies. Epool uses only standard OTP, no other application is needed and will ever be.

## Authors

- Madalin Grigore-Enescu (ergenius) <os@ergenius.com>

## License

Epool is available in the public domain.

Epool is also optionally available under the MIT license (see `LICENSE`) for jurisdictions that do not recognize public domain works.

%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Implements operations for idle workers based on erlang lists.
%%% @end
%%%-------------------------------------------------------------------
-module(epool_x_idle_workers_list).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([new/0]).

-export([add/2, add/3]).

-export([take_head/1]).
-export([take_by_worker_pid/2]).
-export([take_by_worker_monitor_ref/2]).

%% Type definition for idle workers list
-type epool_idle_workers() :: [epool_idle_worker()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates and return a new, empty busy workers list
new() -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified worker to the front of idle workers list
%% Attach the element to the beginning of the list which is faster
-spec add(IdleWorker, IdleWorkers) -> IdleWorkers2 when
    IdleWorker      :: epool_idle_worker(),
    IdleWorkers     :: epool_idle_workers(),
    IdleWorkers2    :: epool_idle_workers().

add(IdleWorker, IdleWorkers) -> [IdleWorker | IdleWorkers].

%% @doc Add the specified worker to the idle workers list with custom strategy
-spec add(Strategy, IdleWorker, IdleWorkers) -> IdleWorkers2 when
    Strategy        :: lifo | fifo | random,
    IdleWorker      :: epool_idle_worker(),
    IdleWorkers     :: epool_idle_workers(),
    IdleWorkers2    :: epool_idle_workers().

add(lifo, IdleWorker, IdleWorkers) -> [IdleWorker | IdleWorkers];
add(fifo, IdleWorker, IdleWorkers) -> IdleWorkers ++ [IdleWorker];
add(random, IdleWorker, IdleWorkers) ->
    case (trunc(rand:uniform() * 2) + 1) of
        1 -> add(lifo, IdleWorker, IdleWorkers);
        2 -> add(fifo, IdleWorker, IdleWorkers)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take the worker from the head of the idle workers if any
-spec take_head(IdleWorkers) -> {IdleWorker, IdleWorkers2} | empty when
    IdleWorkers     :: epool_idle_workers(),
    IdleWorker      :: epool_idle_worker(),
    IdleWorkers2    :: epool_idle_workers().

take_head([IdleWorker | IdleWorkers2]) -> {IdleWorker, IdleWorkers2};
take_head([]) -> empty.

%% @doc Take the specified worker from the idle workers by worker pid
-spec take_by_worker_pid(WorkerPid, IdleWorkers) -> IdleWorker | not_found when
    WorkerPid   :: pid(),
    IdleWorkers :: epool_idle_workers(),
    IdleWorker  :: epool_idle_worker().

take_by_worker_pid(WorkerPid, IdleWorkers) ->

    case get_by_worker_pid(WorkerPid, IdleWorkers) of
        not_found -> not_found;
        IdleWorker -> {IdleWorker, delete_by_worker_pid(WorkerPid, IdleWorkers)}
    end.

%% @doc Take the specified worker from the idle workers by worker monitor reference
-spec take_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers) -> IdleWorker | not_found when
    WorkerMonitorRef    :: reference(),
    IdleWorkers         :: epool_idle_workers(),
    IdleWorker          :: epool_idle_worker().

take_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers) ->

    case get_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers) of
        not_found -> not_found;
        BusyWorker -> {BusyWorker, delete_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the worker with worker pid `WorkerPid` from idle workers `IdleWorkers`.
%% Returns the idle worker if found or `not_found` otherwise.
-spec get_by_worker_pid(WorkerPid, IdleWorkers) -> IdleWorker | not_found when
    WorkerPid   :: pid(),
    IdleWorkers :: epool_idle_workers(),
    IdleWorker  :: epool_idle_worker().

get_by_worker_pid(WorkerPid, [IdleWorker = #epool_idle_worker{worker_pid = WorkerPid} | _]) -> IdleWorker;
get_by_worker_pid(WorkerPid, [_ | T]) -> get_by_worker_pid(WorkerPid, T);
get_by_worker_pid(_WorkerPid, []) -> not_found.

%% @doc Returns the worker with worker monitor reference `WorkerMonitorRef` from idle workers `IdleWorkers`.
%% Returns the idle worker if found or `not_found` otherwise.
-spec get_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers) -> IdleWorker | not_found when
    WorkerMonitorRef    :: reference(),
    IdleWorkers         :: epool_idle_workers(),
    IdleWorker          :: epool_idle_worker().

get_by_worker_monitor_ref(WorkerMonitorRef, [IdleWorker = #epool_idle_worker{worker_monitor_ref = WorkerMonitorRef} | _]) -> IdleWorker;
get_by_worker_monitor_ref(WorkerMonitorRef, [_ | T]) -> get_by_worker_monitor_ref(WorkerMonitorRef, T);
get_by_worker_monitor_ref(_WorkerMonitorRef, []) -> not_found.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete the worker with worker pid `WorkerPid` from idle workers `IdleWorkers`.
-spec delete_by_worker_pid(WorkerPid, IdleWorkers) -> IdleWorkers2 when
    WorkerPid       :: pid(),
    IdleWorkers     :: epool_idle_workers(),
    IdleWorkers2    :: epool_idle_workers().

delete_by_worker_pid(WorkerPid, [#epool_idle_worker{worker_pid = WorkerPid} | T]) -> T;
delete_by_worker_pid(WorkerPid, [H | T]) -> [H | delete_by_worker_pid(WorkerPid, T)];
delete_by_worker_pid(_WorkerPid, []) -> [].

%% @doc Delete the worker with worker monitor reference `WorkerMonitorRef` from idle workers `IdleWorkers`.
-spec delete_by_worker_monitor_ref(WorkerMonitorRef, IdleWorkers) -> IdleWorkers2 when
    WorkerMonitorRef    :: reference(),
    IdleWorkers         :: epool_idle_workers(),
    IdleWorkers2        :: epool_idle_workers().

delete_by_worker_monitor_ref(WorkerMonitorRef, [#epool_idle_worker{worker_monitor_ref = WorkerMonitorRef} | T]) -> T;
delete_by_worker_monitor_ref(WorkerMonitorRef, [H | T]) -> [H | delete_by_worker_monitor_ref(WorkerMonitorRef, T)];
delete_by_worker_monitor_ref(_WorkerMonitorRef, []) -> [].


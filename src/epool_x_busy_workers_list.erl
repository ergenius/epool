%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Implements operations for busy workers based on erlang lists.
%%%
%%% This implementation is best suited for pools with relative low number of workers (below 100).
%%% Best usage for this implementation are database driver clients pools. Usually database drivers
%%% maintain a low number of clients connected to a database node (around 10 - 20) so this is all
%%% you will iterate and copy all around.
%%%
%%% If you need to create a huge amount of workers try ETS implementations. Those are better suited for the purpose.
%%% ETS implementations only returns a ets:tid() back to the caller. The epool_srv_pool will copy only this integer tid
%%% not entire lists when you alter the state. This can hardly improve your pool performance.
%%% @end
%%%-------------------------------------------------------------------
-module(epool_x_busy_workers_list).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([new/0]).

-export([add/2]).

-export([take_by_worker_pid/2]).
-export([take_by_monitor_ref/2]).
-export([take_by_caller_ref/2]).

%% Type definition for busy workers list
-type epool_busy_workers() :: [epool_busy_worker()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates and return a new, empty busy workers list
new() -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified worker to the busy workers list
%% Attach the element to the beginning of the list which is faster
-spec add(BusyWorker, BusyWorkers) -> BusyWorkers2 when
    BusyWorker      :: epool_busy_worker(),
    BusyWorkers     :: epool_busy_workers(),
    BusyWorkers2    :: epool_busy_workers().
add(BusyWorker, BusyWorkers) -> [BusyWorker | BusyWorkers].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take the specified worker from the busy workers by worker pid
-spec take_by_worker_pid(WorkerPid, BusyWorkers) -> BusyWorker | not_found when
    WorkerPid   :: pid(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_worker_pid(WorkerPid, BusyWorkers) ->

    case get_by_worker_pid(WorkerPid, BusyWorkers) of
        not_found -> not_found;
        BusyWorker -> {BusyWorker, delete_by_worker_pid(WorkerPid, BusyWorkers)}
    end.

%% @doc Take the specified worker from the busy workers by caller reference
-spec take_by_caller_ref(CallerRef, BusyWorkers) -> BusyWorker | not_found when
    CallerRef   :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_caller_ref(CallerRef, BusyWorkers) ->

    case get_by_caller_ref(CallerRef, BusyWorkers) of
        not_found -> not_found;
        BusyWorker -> {BusyWorker, delete_by_caller_ref(CallerRef, BusyWorkers)}
    end.

%% @doc Take the specified worker from the busy workers by monitor reference
%% This function try to match either worker or caller monitor ref
-spec take_by_monitor_ref(MonitorRef, BusyWorkers) -> BusyWorker | not_found when
    MonitorRef  :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_monitor_ref(MonitorRef, BusyWorkers) ->

    case get_by_monitor_ref(MonitorRef, BusyWorkers) of
        not_found -> not_found;
        BusyWorker -> {BusyWorker, delete_by_monitor_ref(MonitorRef, BusyWorkers)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the worker with worker pid `WorkerPid` from busy workers `BusyWorkers`.
%% Returns the busy worker if found or `not_found` otherwise.
-spec get_by_worker_pid(WorkerPid, BusyWorkers) -> BusyWorker | not_found when
    WorkerPid   :: pid(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

get_by_worker_pid(WorkerPid, [BusyWorker = #epool_busy_worker{worker_pid = WorkerPid} | _]) -> BusyWorker;
get_by_worker_pid(WorkerPid, [_ | T]) -> get_by_worker_pid(WorkerPid, T);
get_by_worker_pid(_WorkerPid, []) -> not_found.

%% @doc Returns the worker with caller reference `CallerRef` from busy workers `BusyWorkers`.
%% Returns the busy worker if found or `not_found` otherwise.
-spec get_by_caller_ref(CallerRef, BusyWorkers) -> BusyWorker | not_found when
    CallerRef   :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

get_by_caller_ref(CallerRef, [BusyWorker = #epool_busy_worker{caller_ref = CallerRef} | _]) -> BusyWorker;
get_by_caller_ref(CallerRef, [_ | T]) -> get_by_caller_ref(CallerRef, T);
get_by_caller_ref(_CallerRef, []) -> not_found.

%% @doc Returns the worker with monitor reference `MonitorRef` from busy workers `BusyWorkers`.
%% This function try to match either worker or caller monitor ref.
%% Returns the busy worker if found or `not_found` otherwise.
-spec get_by_monitor_ref(MonitorRef, BusyWorkers) -> BusyWorker | not_found when
    MonitorRef  :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

get_by_monitor_ref(MonitorRef, [BusyWorker = #epool_busy_worker{
    worker_monitor_ref = WorkerMonitorRef,
    caller_monitor_ref = CallerMonitorRef} | T]) ->
    case MonitorRef of
        WorkerMonitorRef -> BusyWorker;
        CallerMonitorRef -> BusyWorker;
        _ -> get_by_caller_ref(MonitorRef, T)
    end;
get_by_monitor_ref(_MonitorRef, []) -> not_found.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete the worker with worker pid `WorkerPid` from busy workers `BusyWorkers`.
-spec delete_by_worker_pid(WorkerPid, BusyWorkers) -> BusyWorkers2 when
    WorkerPid       :: pid(),
    BusyWorkers     :: epool_busy_workers(),
    BusyWorkers2    :: epool_busy_workers().

delete_by_worker_pid(WorkerPid, [#epool_busy_worker{worker_pid = WorkerPid} | T]) -> T;
delete_by_worker_pid(WorkerPid, [H | T]) -> [H | delete_by_worker_pid(WorkerPid, T)];
delete_by_worker_pid(_WorkerPid, []) -> [].

%% @doc Delete the worker with worker pid `WorkerPid` from busy workers `BusyWorkers`.
-spec delete_by_caller_ref(CallerRef, BusyWorkers) -> BusyWorkers2 when
    CallerRef       :: reference(),
    BusyWorkers     :: epool_busy_workers(),
    BusyWorkers2    :: epool_busy_workers().

delete_by_caller_ref(CallerRef, [#epool_busy_worker{caller_ref = CallerRef} | T]) -> T;
delete_by_caller_ref(CallerRef, [H | T]) -> [H | delete_by_caller_ref(CallerRef, T)];
delete_by_caller_ref(_CallerRef, []) -> [].

%% @doc Delete the worker with worker pid `WorkerPid` from busy workers `BusyWorkers`.
%% This function try to match either worker or caller monitor ref
-spec delete_by_monitor_ref(MonitorRef, BusyWorkers) -> BusyWorkers2 when
    MonitorRef      :: reference(),
    BusyWorkers     :: epool_busy_workers(),
    BusyWorkers2    :: epool_busy_workers().

delete_by_monitor_ref(MonitorRef, [BusyWorker = #epool_busy_worker{
    worker_monitor_ref = WorkerMonitorRef,
    caller_monitor_ref = CallerMonitorRef} | T]) ->
    case MonitorRef of
        WorkerMonitorRef -> T;
        CallerMonitorRef -> T;
        _ -> [BusyWorker | delete_by_monitor_ref(MonitorRef, T)]
    end;
delete_by_monitor_ref(_MonitorRef, []) -> [].


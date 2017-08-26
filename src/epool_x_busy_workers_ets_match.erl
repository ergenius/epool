%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Implements operations for busy workers based on erlang ETS using only one ETS table and ets match patterns.
%%%
%%% A separate implementation of the busy workers using 3 ETS table and only ets:lookup function is provided for you
%%% in `epool_lib_busy_workers_et_lookup` module.
%%% @end
%%%-------------------------------------------------------------------
-module(epool_x_busy_workers_ets_match).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([new/0]).

-export([add/2]).

-export([take_by_worker_pid/2]).
-export([take_by_monitor_ref/2]).
-export([take_by_caller_ref/2]).

%% Type definition for busy workers list
%% A table identifier, as returned by new/2
-type epool_busy_workers() :: ets:tid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates and return a new, empty busy workers list
new() -> ets:new(ets_workers, [set, private]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified worker to the busy workers list
%% Attach the element to the beginning of the list which is faster
-spec add(BusyWorker, BusyWorkers) -> BusyWorkers2 when
    BusyWorker      :: epool_busy_worker(),
    BusyWorkers     :: epool_busy_workers(),
    BusyWorkers2    :: epool_busy_workers().
add(#epool_busy_worker{
    worker_pid              = WorkerPid,
    worker_monitor_ref      = WorkerMonitorRef,
    worker_creation_time    = WorkerCreationTime,
    caller_pid              = CallerPid,
    caller_ref              = CallerRef,
    caller_monitor_ref      = CallerMonitorRef
}, BusyWorkers) ->

    true = ets:insert(BusyWorkers, {WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerRef, CallerMonitorRef}),
    BusyWorkers.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take the specified worker from the busy workers by worker pid
-spec take_by_worker_pid(WorkerPid, BusyWorkers) -> BusyWorker | not_found when
    WorkerPid   :: pid(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_worker_pid(WorkerPid, BusyWorkers) ->

    case ets:lookup(BusyWorkers, WorkerPid) of
        [{WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerRef, CallerMonitorRef}] ->

            %% Remove the busy worker from ets
            true = ets:delete(BusyWorkers, WorkerPid),

            %% Returns the busy worker
            {#epool_busy_worker{
                worker_pid              = WorkerPid,
                worker_monitor_ref      = WorkerMonitorRef,
                worker_creation_time    = WorkerCreationTime,
                caller_pid              = CallerPid,
                caller_ref              = CallerRef,
                caller_monitor_ref      = CallerMonitorRef
            }, BusyWorkers};

        _ -> not_found
    end.

%% @doc Take the specified worker from the busy workers by caller reference
-spec take_by_caller_ref(CallerRef, BusyWorkers) -> BusyWorker | not_found when
    CallerRef   :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_caller_ref(CallerRef, BusyWorkers) ->

    %% Locate the worker
    %% Stop on first match since the table doesn't contain any duplicate caller reference
    case ets:match(BusyWorkers, {'$1', '$2', '$3', '$4', CallerRef, '$5'}, 1) of
        {[{WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerMonitorRef}], _Continuation} ->

            %% Remove the busy worker from ets
            true = ets:delete(BusyWorkers, WorkerPid),

            %% Returns the busy worker
            {#epool_busy_worker{
                worker_pid              = WorkerPid,
                worker_monitor_ref      = WorkerMonitorRef,
                worker_creation_time    = WorkerCreationTime,
                caller_pid              = CallerPid,
                caller_ref              = CallerRef,
                caller_monitor_ref      = CallerMonitorRef
            }, BusyWorkers};

        _ -> not_found
    end.

%% @doc Take the specified worker from the busy workers by monitor reference
%% This function try to match either worker or caller monitor ref
-spec take_by_monitor_ref(MonitorRef, BusyWorkers) -> BusyWorker | not_found when
    MonitorRef  :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_monitor_ref(MonitorRef, BusyWorkers) ->

    %% Locate the worker
    %% Stop on first match since the table doesn't contain any duplicate monitor reference
    case ets:select(BusyWorkers, [{{'_', MonitorRef, '_', '_', '_', '_'}, [], ['$_']}, {{'_', '_', '_', '_', '_', MonitorRef}, [], ['$_']}], 1) of
        {[{WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerRef, CallerMonitorRef}], _Continuation} ->

            %% Remove the busy worker from ets
            true = ets:delete(BusyWorkers, WorkerPid),

            %% Returns the busy worker
            {#epool_busy_worker{
                worker_pid              = WorkerPid,
                worker_monitor_ref      = WorkerMonitorRef,
                worker_creation_time    = WorkerCreationTime,
                caller_pid              = CallerPid,
                caller_ref              = CallerRef,
                caller_monitor_ref      = CallerMonitorRef
            }, BusyWorkers};

        _ -> not_found
    end.
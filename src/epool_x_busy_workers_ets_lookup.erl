%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Implements operations for busy workers based on erlang ETS using 2 extra index ETS tables for lookup.
%%%
%%% Notice that when calling ets:match if the key is not specified,
%%% that is, if it is a variable or an underscore, the entire table must be searched.
%%% The search time can be substantial if the table is very large.
%%%
%%% Also notice that the only recommended usage of the busy workers implementations based on ETS is for very LARGE amount of workers.
%%% For pools containing minimal amount of workers is much better to use the busy workers implementation based on lists or other erlang data types.
%%%
%%% This module provide a busy workers implementation that works only by using ets:lookup function.
%%% In order to achieve this we create and use 3 different lookup tables similar to databases tables indexes.
%%%
%%% A separate implementation of the busy workers using only one ETS table and ets match patterns is provided for you to test
%%% in `epool_lib_busy_workers_ets_match` module.
%%% @end
%%%-------------------------------------------------------------------
-module(epool_x_busy_workers_ets_lookup).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([new/0]).

-export([add/2]).

-export([take_by_worker_pid/2]).
-export([take_by_monitor_ref/2]).
-export([take_by_caller_ref/2]).

%% Busy workers record
-record(epool_busy_workers, {
    ets_workers           :: ets:tid(),
    ets_index_caller_ref  :: ets:tid(),
    ets_index_monitor_ref :: ets:tid()
}).

%% Type definition for busy workers list
%% A table identifier, as returned by new/2
-type epool_busy_workers() :: #epool_busy_workers{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates and return a new, empty busy workers list
new() ->

    %% Optimize lookups by creating 3 different lookup tables similar to databases tables indexes.
    %% This way we avoid using ets:match that fully iterate a single table.
    #epool_busy_workers{
        ets_workers           = ets:new(ets_workers, [set, private]),
        ets_index_caller_ref  = ets:new(ets_index_caller_ref, [set, private]),
        ets_index_monitor_ref = ets:new(ets_index_monitor_ref, [set, private])
    }.

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
}, BusyWorkers = #epool_busy_workers{
    ets_workers             = EtsWorkers,
    ets_index_caller_ref    = EtsIndexCallerRef,
    ets_index_monitor_ref   = EtsIndexMonitorRef
}) ->

    true = ets:insert(EtsWorkers, {WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerRef, CallerMonitorRef}),
    true = ets:insert(EtsIndexCallerRef, {CallerRef, WorkerPid}),
    true = ets:insert(EtsIndexMonitorRef, [{WorkerMonitorRef, WorkerPid}, {CallerMonitorRef, WorkerPid}]),
    BusyWorkers.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take the specified worker from the busy workers by worker pid
-spec take_by_worker_pid(WorkerPid, BusyWorkers) -> BusyWorker | not_found when
    WorkerPid   :: pid(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_worker_pid(WorkerPid, BusyWorkers = #epool_busy_workers{
    ets_workers             = EtsWorkers,
    ets_index_caller_ref    = EtsIndexCallerRef,
    ets_index_monitor_ref   = EtsIndexMonitorRef
}) ->

    case ets:lookup(EtsWorkers, WorkerPid) of
        [{WorkerPid, WorkerMonitorRef, WorkerCreationTime, CallerPid, CallerRef, CallerMonitorRef}] ->

            %% Remove the busy worker from all ets including indexes ets
            true = ets:delete(EtsWorkers, WorkerPid),
            true = ets:delete(EtsIndexCallerRef, CallerRef),
            true = ets:delete(EtsIndexMonitorRef, WorkerMonitorRef),
            true = ets:delete(EtsIndexMonitorRef, CallerMonitorRef),

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

take_by_caller_ref(CallerRef, BusyWorkers = #epool_busy_workers{
    ets_index_caller_ref    = EtsIndexCallerRef
}) ->

    %% Locate the worker
    case ets:lookup(EtsIndexCallerRef, CallerRef) of
        [{CallerRef, WorkerPid}] ->

            %% Take the worker out by worker pid
            take_by_worker_pid(WorkerPid, BusyWorkers);

        _ -> not_found
    end.

%% @doc Take the specified worker from the busy workers by monitor reference
%% This function try to match either worker or caller monitor ref
-spec take_by_monitor_ref(MonitorRef, BusyWorkers) -> BusyWorker | not_found when
    MonitorRef  :: reference(),
    BusyWorkers :: epool_busy_workers(),
    BusyWorker  :: epool_busy_worker().

take_by_monitor_ref(MonitorRef, BusyWorkers = #epool_busy_workers{
    ets_index_monitor_ref   = EtsIndexMonitorRef
}) ->

    %% Locate the worker
    case ets:lookup(EtsIndexMonitorRef, MonitorRef) of
        [{MonitorRef, WorkerPid}] ->

            %% Take the worker out by worker pid
            take_by_worker_pid(WorkerPid, BusyWorkers);

        _ -> not_found
    end.
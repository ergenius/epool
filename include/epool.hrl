%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

%% Default configuration options when they are not provided

%% For more details on supervisor restart intensity and period please consult: https://github.com/erlang/otp/pull/1289/files
-define(EPOOL_DEFAULT_SUP_NAME,                         {local, epool_sup}).
-define(EPOOL_DEFAULT_SUP_REF,                          epool_sup).
-define(EPOOL_DEFAULT_SUP_SHUTDOWN,                     5000).
-define(EPOOL_DEFAULT_SUP_RESTART_INTENSITY,            10).
-define(EPOOL_DEFAULT_SUP_RESTART_PERIOD,               10).

-define(EPOOL_DEFAULT_SRV_GROUPING_NAME,                {local, epool_srv_grouping}).
-define(EPOOL_DEFAULT_SRV_GROUPING_REF,                 epool_srv_grouping).
-define(EPOOL_DEFAULT_SRV_GROUPING_SHUTDOWN,            5000).

-define(EPOOL_DEFAULT_SUP_POOL_SHUTDOWN,                5000).
-define(EPOOL_DEFAULT_SUP_POOL_RESTART_INTENSITY,       10).
-define(EPOOL_DEFAULT_SUP_POOL_RESTART_PERIOD,          10).

-define(EPOOL_DEFAULT_SUP_WORKERS_SHUTDOWN,             5000).
-define(EPOOL_DEFAULT_SUP_WORKERS_RESTART_INTENSITY,    10).
-define(EPOOL_DEFAULT_SUP_WORKERS_RESTART_PERIOD,       10).

-define(EPOOL_DEFAULT_SRV_POOL_SHUTDOWN,                5000).
-define(EPOOL_DEFAULT_SRV_WORKERS_MANAGER_SHUTDOWN,     5000).

-define(EPOOL_DEFAULT_MIN_SIZE,                         5).
-define(EPOOL_DEFAULT_MAX_SIZE,                         10).
-define(EPOOL_DEFAULT_RETRY_CREATING,                   forever).
-define(EPOOL_DEFAULT_RETRY_CREATING_INTERVAL,          30000).
-define(EPOOL_DEFAULT_SUPERVISION,                      default).
-define(EPOOL_DEFAULT_STRATEGY,                         lifo).
-define(EPOOL_DEFAULT_IDLE_WORKERS_MODULE,              epool_x_idle_workers_list).
-define(EPOOL_DEFAULT_BUSY_WORKERS_MODULE,              epool_x_busy_workers_list).
-define(EPOOL_DEFAULT_WAITING_CALLS_MODULE,             epool_x_waiting_calls_list).

%% Various ETS names
-define(EPOOL_ETS_GROUPING_POOLS_BY_GROUP,              epool_ets_grouping_pools_by_group).

%% Epool time unit type
%% Supports original pooler time unit format and Erlang common time units.
-type epool_time_unit() ::  min | sec | ms | mu | minute | second | millisecond | microsecond.

%% Epool time specification
-type epool_time_spec() :: non_neg_integer() | {non_neg_integer(), epool_time_unit()}.

%% Type definition for pool name
-type epool_name() :: atom() | {local, atom()} | {global, term()} | {via, atom(), term()}.

%% Type definition for pool group name
-type epool_group_name() :: atom().

%% Epool create configuration
-record(epool_config, {

    %% Pool name
    %% mandatory
    name                            = undefined :: epool_name(),

    %% Pool groups
    %% optional
    %% Defaults to 'undefined'
    %% You can group pools by group names. If no pool group name is specified
    %% the pool will not be added to any group. Epool implements both pooler and poolboy approaches.
    %% If you want to use groups, you can easily do it. If you don't, there will be no performance penalty for supporting groups
    %% if you don't define one. You can also add a pool to multiple groups once.
    groups                          = undefined :: [atom()],

    %% Pool minimal size
    %% optional
    %% Defaults to ?EPOOL_DEFAULT_MIN_SIZE
    %% Specify how many minimal amount of workers the pool will maintain.
    min_size                        = ?EPOOL_DEFAULT_MIN_SIZE :: non_neg_integer(),

    %% Pool maximum size
    %% optional
    %% Defaults to ?EPOOL_DEFAULT_MAX_SIZE
    %% Specify how many maximum amount of workers the pool will create when there are no more free workers in the pool to take.
    max_size                        = ?EPOOL_DEFAULT_MAX_SIZE :: non_neg_integer(),

    %% Retry creating workers on failure
    retry_creating                  = ?EPOOL_DEFAULT_RETRY_CREATING :: forever | never | non_neg_integer(),

    %% Interval used to retry creating workers
    retry_creating_interval         = ?EPOOL_DEFAULT_RETRY_CREATING_INTERVAL :: non_neg_integer(),

    %% Pool take and release strategy
    %% optional
    %% Defaults to lifo
    %% Specify the strategy used to take workers from the pool and release them back to the pool.
    %% - lifo - Last In, First Out
    %% - fifo - First In, First Out
    %% - random - random
    strategy                        = lifo :: lifo | fifo | random,

    %% Culling interval
    culling_interval                = undefined :: undefined | epool_time_spec(),

    %% Culling age
    culling_age                     = undefined :: undefined | epool_time_spec(),

    %% Pool workers supervisor shutdown time
    %% optional
    %% shutdown defines how the pool processes must be terminated.
    %% brutal_kill means that the process is unconditionally terminated using exit(Child,kill).
    %% An integer time-out value means that the supervisor tells the child process to terminate by calling exit(Child,shutdown)
    %% and then wait for an exit signal with reason shutdown back from the child process.
    %% If no exit signal is received within the specified number of milliseconds, the child process is unconditionally terminated using exit(Child,kill).
    %% The shutdown time can also be set to infinity to give the pool ample time to shut down.
    sup_pool_shutdown               = ?EPOOL_DEFAULT_SUP_POOL_SHUTDOWN :: infinity | brutal_kill | non_neg_integer(),

    %% Pool workers supervisor restart intensity
    sup_pool_restart_intensity      = ?EPOOL_DEFAULT_SUP_POOL_RESTART_INTENSITY :: non_neg_integer(),

    %% Pool workers supervisor restart period
    sup_pool_restart_period         = ?EPOOL_DEFAULT_SUP_POOL_RESTART_PERIOD :: non_neg_integer(),

    %% Pool workers supervisor shutdown time
    %% optional
    %% shutdown defines how the pool processes must be terminated.
    %% brutal_kill means that the process is unconditionally terminated using exit(Child,kill).
    %% An integer time-out value means that the supervisor tells the child process to terminate by calling exit(Child,shutdown)
    %% and then wait for an exit signal with reason shutdown back from the child process.
    %% If no exit signal is received within the specified number of milliseconds, the child process is unconditionally terminated using exit(Child,kill).
    %% The shutdown time can also be set to infinity to give the pool ample time to shut down.
    sup_workers_shutdown            = ?EPOOL_DEFAULT_SUP_WORKERS_SHUTDOWN :: infinity | brutal_kill | non_neg_integer(),

    %% Pool workers supervisor restart intensity
    sup_workers_restart_intensity   = ?EPOOL_DEFAULT_SUP_WORKERS_RESTART_INTENSITY :: non_neg_integer(),

    %% Pool workers supervisor restart period
    sup_workers_restart_period      = ?EPOOL_DEFAULT_SUP_WORKERS_RESTART_PERIOD :: non_neg_integer(),

    %% Pool server shutdown time
    srv_pool_shutdown               = ?EPOOL_DEFAULT_SRV_POOL_SHUTDOWN :: infinity | brutal_kill | non_neg_integer(),

    %% Pool workers manager server shutdown time
    srv_workers_manager_shutdown    = ?EPOOL_DEFAULT_SRV_WORKERS_MANAGER_SHUTDOWN :: infinity | brutal_kill | non_neg_integer(),

    %% Worker module
    %% mandatory
    %% Undefined worker module name will trigger an exception into the pool creation.
    worker_module                   = undefined :: atom(),

    %% Worker arguments
    %% optional
    %% Defaults to `undefined`
    worker_args                     = undefined :: term(),

    %% Idle workers module
    %% optional
    %% Defaults to ?EPOOL_DEFAULT_IDLE_WORKERS_MODULE
    idle_workers_module             = ?EPOOL_DEFAULT_IDLE_WORKERS_MODULE :: atom(),

    %% Busy workers module
    %% optional
    %% Defaults to ?EPOOL_DEFAULT_BUSY_WORKERS_MODULE
    busy_workers_module             = ?EPOOL_DEFAULT_BUSY_WORKERS_MODULE :: atom(),

    %% Waiting calls module
    %% optional
    %% Defaults to ?EPOOL_DEFAULT_WAITING_CALLS_MODULE
    waiting_calls_module            = ?EPOOL_DEFAULT_WAITING_CALLS_MODULE :: atom()

}).

%% Type definition for epool config
-type epool_config() :: #epool_config{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% idle workers, busy workers and waiting calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Idle worker record
-record(epool_idle_worker, {

    %% Worker pid
    worker_pid              :: pid(),

    %% Worker monitor reference
    worker_monitor_ref      :: reference(),

    %% Time the worker was created in milliseconds
    worker_creation_time    :: non_neg_integer(),

    %% Last time the worker was added to the idle workers in seconds
    worker_idle_time        :: non_neg_integer()
}).

%% Busy worker record
-record(epool_busy_worker, {
    worker_pid              :: pid(),
    worker_monitor_ref      :: reference(),
    worker_creation_time    :: non_neg_integer(),
    caller_pid              :: pid(),
    caller_ref              :: reference(),
    caller_monitor_ref      :: reference()
}).

%% Waiting call record
-record(epool_waiting_call, {
    caller_pid          :: pid(),
    caller_ref          :: reference(),
    caller_monitor_ref  :: reference()
}).

%% Type definition for a idle worker
-type epool_idle_worker() :: #epool_idle_worker{}.

%% Type definition for a busy worker
-type epool_busy_worker() :: #epool_busy_worker{}.

%% Type definition for a waiting call
-type epool_waiting_call() :: #epool_waiting_call{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% states & other records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Record holding epool statistics
-record(epool_stats, {

    %% Total number of fast succesfully take calls
    take_fast_count             = 0 :: non_neg_integer(),

    %% Total number of fast failed take calls
    take_fast_fail_count        = 0 :: non_neg_integer(),

    %% Total number of waiting take calls
    take_waiting_count          = 0 :: non_neg_integer(),

    %% Total number of take calls that where responded later
    take_later_count            = 0 :: non_neg_integer(),

    %% Total number of take cancel (take timeouts or errors)
    take_cancel_count           = 0 :: non_neg_integer(),

    %% Total number of unknown take cancel
    %% (cancel take for calls that no longer exist in waiting calls list)
    take_cancel_unknown_count   = 0 :: non_neg_integer(),

    %% Total number of worker release calls
    release_count               = 0 :: non_neg_integer(),

    %% Total number of unknown worker release calls
    release_unknown_count       = 0 :: non_neg_integer(),

    %% Total number of DOWN callers since the creation of the pool
    down_callers_count          = 0 :: non_neg_integer(),

    %% Total number of DOWN workers
    down_workers_count          = 0 :: non_neg_integer(),

    %% Total number of unknown DOWN info received by the pool
    down_unknown_count          = 0 :: non_neg_integer(),

    %% Total number of new workers added to the pool
    add_workers_count           = 0 :: non_neg_integer()

}).

%% Type definition for epool stats
-type epool_stats() :: #epool_stats{}.

%% Record holding epool status
-record(epool_status, {

    %% Pool size
    size                        = 0 :: non_neg_integer(),

    %% Pool requested size
    requested_size              = 0 :: non_neg_integer(),

    %% Pool statistics
    stats                       = undefined :: undefined | epool_stats()

}).

%% Type definition for epool_status
-type epool_status() :: #epool_status{}.

%% Pools grouping server state
-record(epool_srv_grouping_state, {
    monitors = [] :: list()
}).

%% Type definition for pool server state
-type epool_srv_grouping_state() :: #epool_srv_grouping_state{}.

%% Pool server state
-record(epool_srv_pool_state, {

    %% Pool name
    name                        = undefined :: epool_name(),

    %% Pool size
    %% Holds the number of the workers in the pool
    size                        = 0 :: non_neg_integer(),

    %% Pool requested size
    %% Holds the number of the workers in the pool (size) plus the
    %% number of the workers requested to be created by the workers manager
    %% that are not yet 'put' into the pool
    requested_size              = 0 :: non_neg_integer(),

    %% Pool minimal size
    min_size                    = ?EPOOL_DEFAULT_MIN_SIZE :: non_neg_integer(),

    %% Pool maximum size
    max_size                    = ?EPOOL_DEFAULT_MAX_SIZE :: non_neg_integer(),

    %% Culling interval in milliseconds
    culling_interval            = 0 :: non_neg_integer(),

    %% Culling age in seconds
    culling_age                 = 0 :: non_neg_integer(),

    %% Culling is started
    culling_started             = false :: boolean(),

    %% Strategy
    strategy                    = lifo :: lifo | fifo | random,

    %% Workers supervisor
    sup_workers                 = undefined :: atom() | pid(),

    %% Workers manager server
    srv_workers_manager         = undefined :: atom() | pid(),

    %% List of idle workers
    idle_workers                = undefined :: term(),

    %% Module for handling idle workers
    idle_workers_module         = ?EPOOL_DEFAULT_IDLE_WORKERS_MODULE :: atom(),

    %% List of busy workers
    busy_workers                = undefined :: term(),

    %% Module for handling busy workers
    busy_workers_module         = ?EPOOL_DEFAULT_BUSY_WORKERS_MODULE :: atom(),

    %% Holds `take` calls that are waiting for an answer
    waiting_calls               = undefined :: term(),

    %% Module for handling waiting calls
    waiting_calls_module        = ?EPOOL_DEFAULT_WAITING_CALLS_MODULE :: atom(),

    %% Pool statistics
    stats                       = #epool_stats{} :: epool_stats()

}).

%% Type definition for pool server state
-type epool_srv_pool_state() :: #epool_srv_pool_state{}.

%% Pool pool workers manager server state
-record(epool_srv_workers_manager_state, {

    %% Pool server
    srv_pool        :: atom() | pid(),

    %% Workers supervisor
    sup_workers     :: atom() | pid(),

    %% Retry creating workers on failure
    retry_creating  :: forever | never | non_neg_integer(),

    %% Interval used to retry creating workers
    retry_creating_interval :: non_neg_integer(),

    %% Pool worker module
    worker_module   :: atom(),

    %% Pool worker arguments
    worker_args     :: term()

}).

%% Type definition for pool workers manager server state
-type epool_srv_workers_manager_state() :: #epool_srv_workers_manager_state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% logging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% macros used all over the epool for easily disable error_logger messages in production
-ifdef(epoll_debug).
-define(EPOOL_LOG_INFO_MSG(Format, Data), error_logger:info_msg(Format, Data)).
-define(EPOOL_LOG_WARNING_MSG(Format, Data), error_logger:warning_msg(Format, Data)).
-define(EPOOL_LOG_ERROR_MSG(Format, Data), error_logger:error_msg(Format, Data)).
-define(EPOOL_LOG_INFO_REPORT(Type, Report), error_logger:info_report(Type, Report)).
-define(EPOOL_LOG_WARNING_REPORT(Type, Report), error_logger:warning_report(Type, Report)).
-define(EPOOL_LOG_ERROR_REPORT(Type, Report), error_logger:error_report(Type, Report)).
-else.
-define(EPOOL_LOG_INFO_MSG(Format, Data),       disabled).
-define(EPOOL_LOG_WARNING_MSG(Format, Data),    disabled).
-define(EPOOL_LOG_ERROR_MSG(Format, Data),      disabled).
-define(EPOOL_LOG_INFO_REPORT(Type, Report),    disabled).
-define(EPOOL_LOG_WARNING_REPORT(Type, Report), disabled).
-define(EPOOL_LOG_ERROR_REPORT(Type, Report),   disabled).
-endif.

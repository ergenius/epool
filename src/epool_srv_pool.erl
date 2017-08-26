%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Pool server
%%%
%%% This gen_server maintain workers into the pool, handle take and release pool operations
%%% and receive adding workers to the pool requests send by the workers manager server.
%%%
%%% Pool culling is also performed by this server.
%%% @end
%%% Created : 11. Aug 2017 10:18 AM
%%%-------------------------------------------------------------------
-module(epool_srv_pool).
-author("Madalin Grigore-Enescu").

-behaviour(gen_server).

-include_lib("epool/include/epool.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(PoolConfig :: epool_config()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(PoolConfig = #epool_config{name = Name}) -> gen_server:start_link(Name, ?MODULE, PoolConfig, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(#epool_config{
    name                    = Name,
    min_size                = MinSize,
    max_size                = MaxSize,
    culling_interval        = CullingInterval,
    culling_age             = CullingAge,
    strategy                = Strategy,
    idle_workers_module     = IdleWorkersModule,
    busy_workers_module     = BusyWorkersModule,
    waiting_calls_module    = WaitingCallsModule
}) ->

    %% When a process is trapping exits, it does not terminate when an exit signal is received.
    %% Instead, the signal is transformed into a message {'EXIT',FromPid,Reason}, which is put into the mailbox of the process, just like a regular message.
    %%
    %% An exception to the above is if the exit reason is kill, that is if exit(Pid,kill) has been called.
    %% This unconditionally terminates the process, regardless of if it is trapping exit signals.
    erlang:process_flag(trap_exit, true),

    %% Create busy workers, idle workers, waiting queue
    IdleWorkers  = IdleWorkersModule:new(),
    BusyWorkers  = BusyWorkersModule:new(),
    WaitingCalls = WaitingCallsModule:new(),

    %% Convert time specifications to the required internal state time units
    StateCullingInterval = epool_utils:time_spec_to_milliseconds(CullingInterval),
    StateCullingAge      = epool_utils:time_spec_to_seconds(CullingAge),

    %% Build workers supervisor and workers manager server id
    SupWorkers        = epool_utils:id_sup_workers(Name),
    SrvWorkersManager = epool_utils:id_srv_workers_manager(Name),

    %% Build initial state
    State = #epool_srv_pool_state{

        %% Size
        %% We always start with 0 workers because workers are created later
        %% asynchronously by the workers manager server
        size                = 0,

        %% Pool requested size
        %% Holds the number of the workers requested to be created by the workers manager
        %% that are not yet 'put' into the pool
        requested_size      = 0,

        %% Min pool size
        min_size            = MinSize,

        %% Max pool size
        max_size            = MaxSize,

        %% Culling interval in milliseconds
        culling_interval    = StateCullingInterval,

        %% Culling age in seconds
        culling_age         = StateCullingAge,

        %% Strategy
        strategy            = Strategy,

        %% Workers supervisor
        sup_workers         = SupWorkers,

        %% Workers manager server
        srv_workers_manager = SrvWorkersManager,

        %% Idle workers
        idle_workers        = IdleWorkers,
        idle_workers_module = IdleWorkersModule,

        %% Busy workers
        busy_workers        = BusyWorkers,
        busy_workers_module = BusyWorkersModule,

        %% Waiting queue
        waiting_calls       = WaitingCalls,
        waiting_calls_module= WaitingCallsModule,

        %% Statistics
        stats               = #epool_stats{}

    },

    {ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle take call
handle_call({take, Wait, CallerRef},
            {CallerPid, _},
            State = #epool_srv_pool_state{
                size                    = Size,
                requested_size          = RequestedSize,
                max_size                = MaxSize,
                idle_workers            = IdleWorkers,
                idle_workers_module     = IdleWorkersModule,
                stats                   = Stats
            }) ->

    %% First try to get an idle worker
    case IdleWorkersModule:take_head(IdleWorkers) of

        {#epool_idle_worker{
            worker_pid           = WorkerPid,
            worker_monitor_ref   = WorkerMonitorRef,
            worker_creation_time = WorkerCreationTime
        }, IdleWorkers1} ->

            %% Idle worker found

            %% Monitor the caller process
            CallerMonitorRef = erlang:monitor(process, CallerPid),

            %% Get busy workers
            BusyWorkers         = State#epool_srv_pool_state.busy_workers,
            BusyWorkersModule   = State#epool_srv_pool_state.busy_workers_module,

            %% Add the worker to the busy workers
            BusyWorkers1 = BusyWorkersModule:add(#epool_busy_worker{
                worker_pid           = WorkerPid,
                worker_monitor_ref   = WorkerMonitorRef,
                worker_creation_time = WorkerCreationTime,
                caller_pid           = CallerPid,
                caller_ref           = CallerRef,
                caller_monitor_ref   = CallerMonitorRef
            }, BusyWorkers),

            %% Update the state
            State1 = State#epool_srv_pool_state{
                idle_workers = IdleWorkers1,
                busy_workers = BusyWorkers1,
                stats        = Stats#epool_stats{
                    take_fast_count = Stats#epool_stats.take_fast_count+1
                }},

            %% Give the worker to the caller
            {reply, {ok, WorkerPid}, State1};

        _ ->

            %% No idle worker found

            %% Check limitations to see if we can create more workers
            %%
            %% Creation of more workers may take a lot of time
            %% depends on worker implementation (workers that have slow initialisation/connecting stage).
            %% That's why we create new workers asynchronously.
            %% Otherwise this server will be completely unresponsive until a new worker is created.
            NewRequestedSize = case (Size + RequestedSize) < MaxSize of
                true ->

                    %% Request creation of a new worker asynchronously
                    gen_server:cast(State#epool_srv_pool_state.srv_workers_manager, {create, 1}),

                    %% Increase the requested size
                    RequestedSize + 1;

                _ ->

                    %% Returns requested size unaltered
                    RequestedSize

            end,

            %% Wait if no available workers or return fast otherwise?
            case Wait of
                false ->

                    %% Update the state stats only
                    FastFailState = State#epool_srv_pool_state{
                        requested_size = NewRequestedSize,
                        stats          = Stats#epool_stats{
                            take_fast_fail_count = Stats#epool_stats.take_fast_fail_count + 1
                        }},

                    %% Reply fast to the caller
                    {reply, {error, no_available_worker}, FastFailState};

                _ ->

                    %% Monitor the caller process
                    CallerMonitorRef = erlang:monitor(process, CallerPid),

                    %% Get the waiting calls
                    WaitingCalls        = State#epool_srv_pool_state.waiting_calls,
                    WaitingCallsModule  = State#epool_srv_pool_state.waiting_calls_module,

                    %% Add the call to the waiting calls
                    WaitingCalls2 = WaitingCallsModule:add(#epool_waiting_call{
                        caller_pid         = CallerPid,
                        caller_ref         = CallerRef,
                        caller_monitor_ref = CallerMonitorRef
                    }, WaitingCalls),

                    %% Update the state (and stats)
                    WaitingState = State#epool_srv_pool_state{
                        requested_size  = NewRequestedSize,
                        waiting_calls   = WaitingCalls2,
                        stats           = Stats#epool_stats{
                            take_waiting_count = Stats#epool_stats.take_waiting_count + 1
                        }},

                    %% Reply later when a new worker become available
                    {noreply, WaitingState}

            end

    end;

%% Handle valid add call
%% Add worker valid calls are made by epool_srv_workers_manager
%% after creation of new workers are requested by this server
handle_call({add, WorkerPid, WorkerCreationTime}, _From,
            State = #epool_srv_pool_state{
                size            = Size,
                requested_size  = RequestedSize,
                min_size        = MinSize,
                max_size        = MaxSize,
                culling_started = CullingStarted,
                culling_interval= CullingInterval,
                stats           = Stats
            }) when Size < MaxSize, RequestedSize > 0 ->

    %% Increase the pool size and decrease the requested size
    NewSize          = Size + 1,
    NewRequestedSize = RequestedSize - 1,

    %% Check to see if we need to trigger next culling
    NewCullingStarted = private_start_culling(NewSize, MinSize, CullingStarted, CullingInterval),

    %% Monitor the worker
    WorkerMonitorRef = erlang:monitor(process, WorkerPid),

    %% Get the waiting calls
    WaitingCalls       = State#epool_srv_pool_state.waiting_calls,
    WaitingCallsModule = State#epool_srv_pool_state.waiting_calls_module,

    %% Check if we have waiting calls
    PutOkState = case WaitingCallsModule:take_head(WaitingCalls) of

                     {#epool_waiting_call{
                         caller_pid          = WaitingCallerPid,
                         caller_ref          = WaitingCallerRef,
                         caller_monitor_ref  = WaitingCallerMonitorRef}, WaitingCalls1} ->

                         %% Waiting calls is NOT empty

                         %% Get the busy workers and busy workers module
                         BusyWorkers       = State#epool_srv_pool_state.busy_workers,
                         BusyWorkersModule = State#epool_srv_pool_state.busy_workers_module,

                         %% Add the worker into the busy workers
                         NewBusyWorkers = BusyWorkersModule:add(#epool_busy_worker{
                             worker_pid           = WorkerPid,
                             worker_monitor_ref   = WorkerMonitorRef,
                             worker_creation_time = WorkerCreationTime,
                             caller_pid           = WaitingCallerPid,
                             caller_ref           = WaitingCallerRef,
                             caller_monitor_ref   = WaitingCallerMonitorRef
                         }, BusyWorkers),

                         %% Give the worker to the caller
                         gen_server:reply(WaitingCallerPid, {ok, WorkerPid}),

                         %% Update the state and return
                         State#epool_srv_pool_state{
                             size            = NewSize,
                             requested_size  = NewRequestedSize,
                             busy_workers    = NewBusyWorkers,
                             waiting_calls   = WaitingCalls1,
                             culling_started = NewCullingStarted,
                             stats           = Stats#epool_stats{
                                 take_later_count  = Stats#epool_stats.take_later_count + 1,
                                 add_workers_count = Stats#epool_stats.add_workers_count + 1
                             }};

                     _ ->

                         %% Waiting calls is empty

                         %% Get the pool strategy
                         Strategy            = State#epool_srv_pool_state.strategy,

                         %% Get the pool idle workers
                         IdleWorkers         = State#epool_srv_pool_state.idle_workers,
                         IdleWorkersModule   = State#epool_srv_pool_state.idle_workers_module,

                         %% Put the worker back into idle workers using the strategy used in pool configuration
                         IdleWorkers1 = IdleWorkersModule:add(Strategy, #epool_idle_worker{
                             worker_pid              = WorkerPid,
                             worker_monitor_ref      = WorkerMonitorRef,
                             worker_creation_time    = WorkerCreationTime,
                             worker_idle_time        = epool_utils:time_posix_seconds()
                         }, IdleWorkers),

                         %% Update the state
                         State#epool_srv_pool_state{
                             size            = NewSize,
                             requested_size  = NewRequestedSize,
                             idle_workers    = IdleWorkers1,
                             culling_started = NewCullingStarted,
                             stats           = Stats#epool_stats{
                                 add_workers_count = Stats#epool_stats.add_workers_count + 1
                             }}

                 end,

    %% Reply
    {reply, ok, PutOkState};

%% Handle and log unrequested add calls
handle_call(Msg = {add, _WorkerPid, _WorkerCreationTime}, From,
    State = #epool_srv_pool_state{
        requested_size  = RequestedSize
    }) when RequestedSize < 1 ->

    %% Log the invalid add call
    ?EPOOL_LOG_WARNING_REPORT(epool_add_worker_not_requested, [{msg, Msg}, {from, From}, {state, State}]),

    {reply, {error, not_requested}, State};

%% Handle and log unexpected add calls when pool is full
handle_call(Msg = {add, _WorkerPid, _WorkerCreationTime}, From, State) ->

    %% Log the unexpected add call when pool is full
    ?EPOOL_LOG_WARNING_REPORT(epool_add_full, [{msg, Msg}, {from, From}, {state, State}]),

    {reply, {error, full}, State};

%% Handle status call
handle_call(status, _From, State = #epool_srv_pool_state{
    size            = Size,
    requested_size  = RequestedSize,
    stats           = Stats
}) ->

    %% Returns pool status
    {reply, {ok, #epool_status{
        size            = Size,
        requested_size  = RequestedSize,
        stats           = Stats
    }}, State};

%% Handle stop
handle_call(stop, From, State) ->

    gen_server:reply(From, ok),
    {stop, normal, State};

%% Handle any other call
handle_call(Msg, From, State) ->

    %% Log the unknown call
    ?EPOOL_LOG_WARNING_REPORT(epool_unknown_call, [{msg, Msg}, {from, From}, {state, State}]),

    {reply, {error, unknown_call}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Release a worker back into the pool
handle_cast({release, WorkerPid},
            State = #epool_srv_pool_state{
                busy_workers            = BusyWorkers,
                busy_workers_module     = BusyWorkersModule,
                stats                   = Stats
            }) ->

    %% Take the worker from busy workers list
    case BusyWorkersModule:take_by_worker_pid(WorkerPid, BusyWorkers) of
        {#epool_busy_worker{
            worker_pid           = BusyWorkerPid,
            worker_monitor_ref   = BusyWorkerMonitorRef,
            worker_creation_time = BusyWorkerCreationTime,
            caller_monitor_ref   = CallerMonitorRef
        }, BusyWorkers1} ->

            %% Worker was found in busy workers list

            %% Demonitor the caller
            %% The caller rarely die in this situation so we save some
            %% time by not trying to flush 'DOWN' messages
            true = erlang:demonitor(CallerMonitorRef),

            %% Update the stats
            ReleaseStats = Stats#epool_stats{
                release_count = Stats#epool_stats.release_count + 1
            },

            %% Return the worker back into the pool
            ReleaseBusyWorkerState = private_worker_return(BusyWorkerPid,
                                                        BusyWorkerMonitorRef,
                                                        BusyWorkerCreationTime,
                                                        BusyWorkers1,
                                                        ReleaseStats,
                                                        State),

            {noreply, ReleaseBusyWorkerState};

        _ ->

            %% Worker was not found in busy workers list
            %% This is happening when the worker or the caller process died
            %% and the worker was removed from the busy workers before this cast was handled

            %% Count this situation
            {noreply, State#epool_srv_pool_state{
                stats = Stats#epool_stats{
                    release_unknown_count = Stats#epool_stats.release_unknown_count + 1
                }}}

    end;

%% Cancel the specified take call
handle_cast({take_cancel, CallerRef},
            State = #epool_srv_pool_state{
                waiting_calls           = WaitingCalls,
                waiting_calls_module    = WaitingCallsModule,
                stats                   = Stats
            }) ->

    %% Locate and delete the specified waiting take call
    case WaitingCallsModule:take_by_caller_ref(CallerRef, WaitingCalls) of

        {#epool_waiting_call{
            caller_monitor_ref = CallerMonitorRef
        }, WaitingCalls1} ->

            %% The waiting call was found

            %% Demonitor the caller process
            %%
            %% Flush removes (one) {_, MonitorRef, _, _, _} message, if there is one,
            %% from the caller message queue after monitoring has been stopped to prevent processing
            %% DOWN messages that are no longer important for us. This could happen if the caller process die
            %% in between this cast is handled and the 'DOWN' info is handled in situations where failure
            %% to take a worker conduct to termination of the caller process soon after the failure.
            true = erlang:demonitor(CallerMonitorRef, [flush]),

            %% Update the state and return
            {noreply, State#epool_srv_pool_state{
                waiting_calls = WaitingCalls1,
                stats = Stats#epool_stats{
                    take_cancel_count = Stats#epool_stats.take_cancel_count + 1
                }}};

        _ ->

            %% The caller reference was not found into the waiting calls

            %% This usually indicate that this server was able to reply before
            %% this cancel_take cast arrived and the worker is now placed into
            %% the busy workers list.
            %%
            %% Notice that in this situation the caller will never release the worker
            %% because the call may have failed due to a gen_server:call/3 timeout
            %%
            %% That's why it's necessary to release any worker with the same caller reference
            %% that may be now in the busy workers list.

            %% Get the pool busy workers
            BusyWorkers         = State#epool_srv_pool_state.busy_workers,
            BusyWorkersModule   = State#epool_srv_pool_state.busy_workers_module,

            %% Take it out from the busy workers
            case BusyWorkersModule:take_by_caller_ref(CallerRef, BusyWorkers) of
                {#epool_busy_worker{
                    worker_pid           = BusyWorkerPid,
                    worker_monitor_ref   = BusyWorkerMonitorRef,
                    worker_creation_time = BusyWorkerCreationTime,
                    caller_monitor_ref   = CallerMonitorRef
                    }, BusyWorkers1} ->

                    %% The worker was found into the busy workers

                    %% Demonitor the caller process and delete any 'DOWN'
                    %% that was triggered because of termination of the caller
                    true = erlang:demonitor(CallerMonitorRef, [flush]),

                    %% Update the stats
                    TakeCancelBusyWorkerStats = Stats#epool_stats{
                        take_cancel_count = Stats#epool_stats.take_cancel_count + 1
                    },

                    %% What we do next is to release the worker back into the pool
                    BusyWorkerReturnedState = private_worker_return(BusyWorkerPid,
                                                                    BusyWorkerMonitorRef,
                                                                    BusyWorkerCreationTime,
                                                                    BusyWorkers1,
                                                                    TakeCancelBusyWorkerStats,
                                                                    State),

                    {noreply, BusyWorkerReturnedState};

                _ ->

                    %% The worker was not found
                    %% This could happen if the worker died and was removed from the pool
                    %% before this cast arrived

                    %% Count this situation
                    {noreply, State#epool_srv_pool_state{
                        stats = Stats#epool_stats{
                            take_cancel_unknown_count = Stats#epool_stats.take_cancel_unknown_count + 1
                        }}}

            end

    end;

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle culling
handle_info(cull,
            State = #epool_srv_pool_state{
                size                    = Size,
                min_size                = MinSize,
                culling_interval        = CullingInterval,
                culling_age             = CullingAge,
                idle_workers            = IdleWorkers,
                idle_workers_module     = IdleWorkersModule,
                stats                   = _Stats
            }) ->

    %% Only perform culling when Size > MinSize
    case Size > MinSize of
        true ->

            %% Cull the idle workers by asking the idle workers implementation to do this for us.
            %% This way idle workers implementation can optimize the iteration process as seem fit
            %% according to the storage type.
            case IdleWorkersModule:cull(CullingInterval, CullingAge, Size, MinSize, IdleWorkers) of
                none ->

                    %% Trigger the next cull
                    erlang:send_after(CullingInterval, self(), cull),

                    %% Alter the state
                    CullNoneState = State#epool_srv_pool_state{
                        culling_started = true %% Update send after
                    },

                    {noreply, CullNoneState};

                {HowManyWorkersCulled, CulledIdleWorkers} ->

                    %% Trigger the next cull
                    %% {abs, false}
                    %% This is the default. It means the Time value is interpreted as a time
                    %% in milliseconds relative current Erlang monotonic time.
                    erlang:send_after(CullingInterval, self(), cull),

                    %% Alter the state
                    CullState = State#epool_srv_pool_state{
                        size                = Size - HowManyWorkersCulled,
                        idle_workers        = CulledIdleWorkers,
                        culling_started = true
                    },

                    {noreply, CullState}

            end;

        _ ->

            %% No culling
            %% Save us some time by not triggering the next cull
            %% until new worker are added and Size become bigger than MinSize
            {noreply, State#epool_srv_pool_state{culling_started = false}}

        end;

%% Handle workers or caller exit
handle_info({'DOWN', MonitorRef, process, _, _},
            State = #epool_srv_pool_state{
                busy_workers            = BusyWorkers,
                busy_workers_module     = BusyWorkersModule,
                stats                   = Stats
            }) ->

    %% Notice there is no need to demonitor the monitor reference we receive
    %% because a process monitor is triggered only once,
    %% after that it is removed from both monitoring process and the monitored entity

    %% Search for the monitor reference into the busy workers
    case BusyWorkersModule:take_by_monitor_ref(MonitorRef, BusyWorkers) of
        {BusyWorker, NewBusyWorkers} ->

            %% Who send us the 'DOWN' info:
            %% the busy worker or the caller process?
            case BusyWorker of
                #epool_busy_worker{
                    worker_monitor_ref = MonitorRef,
                    caller_monitor_ref = CallerMonitorRef
                } ->

                    %% BUSY WORKERS -> WORKER DOWN

                    %% Demonitor the caller process
                    true = erlang:demonitor(CallerMonitorRef, [flush]),

                    %% Request creation of a new worker asynchronously
                    NewRequestedSize = State#epool_srv_pool_state.requested_size + 1,
                    gen_server:cast(State#epool_srv_pool_state.srv_workers_manager, create),

                    %% Compute the new pool size
                    NewSize = State#epool_srv_pool_state.size - 1,

                    %% Update the state
                    BusyWorkerDownState = State#epool_srv_pool_state{
                        size            = NewSize,
                        requested_size  = NewRequestedSize,
                        busy_workers    = NewBusyWorkers,
                        stats           = Stats#epool_stats{
                            down_workers_count = Stats#epool_stats.down_workers_count + 1
                        }},

                    %% Return
                    {noreply, BusyWorkerDownState};

                #epool_busy_worker{
                    worker_pid          = BusyWorkerPid,
                    worker_monitor_ref  = BusyWorkerMonitorRef,
                    worker_creation_time= BusyWorkerCreationTime,
                    caller_monitor_ref  = MonitorRef
                } ->

                    %% BUSY WORKERS -> CALLER DOWN

                    %% Alter the stats
                    CallerDownStats = Stats#epool_stats{
                        down_callers_count = Stats#epool_stats.down_callers_count + 1
                    },

                    %% Return the worker back into the pool
                    BusyCallerDownState = private_worker_return(BusyWorkerPid,
                                                                BusyWorkerMonitorRef,
                                                                BusyWorkerCreationTime,
                                                                NewBusyWorkers,
                                                                CallerDownStats,
                                                                State),

                    %% Return
                    {noreply, BusyCallerDownState}

            end;

        _ ->

            %% Get the waiting calls
            WaitingCalls        = State#epool_srv_pool_state.waiting_calls,
            WaitingCallsModule  = State#epool_srv_pool_state.waiting_calls_module,

            %% Search for the caller monitor reference into the waiting calls
            case WaitingCallsModule:take_by_caller_monitor_ref(MonitorRef, WaitingCalls) of
                {_, NewWaitingCalls} ->

                    %% WAITING CALLS -> CALLER DOWN

                    %% Update the new waiting calls into the state
                    DownCallerState = State#epool_srv_pool_state{
                        waiting_calls = NewWaitingCalls,
                        stats         = Stats#epool_stats{
                            down_callers_count = Stats#epool_stats.down_callers_count + 1
                        }},

                    {noreply, DownCallerState};

                _ ->

                    %% Get the idle workers
                    IdleWorkers         = State#epool_srv_pool_state.idle_workers,
                    IdleWorkersModule   = State#epool_srv_pool_state.idle_workers_module,

                    %% Search for the monitor reference into the idle workers
                    case IdleWorkersModule:take_by_worker_monitor_ref(MonitorRef, IdleWorkers) of
                        {_, NewIdleWorkers} ->

                            %% IDLE WORKERS -> WORKER DOWN

                            %% Request creation of a new worker asynchronously
                            NewRequestedSize = State#epool_srv_pool_state.requested_size + 1,
                            gen_server:cast(State#epool_srv_pool_state.srv_workers_manager, create),

                            %% Compute the new pool size
                            NewSize = State#epool_srv_pool_state.size - 1,

                            %% Update the state
                            IdleWorkerDownState = State#epool_srv_pool_state{
                                size            = NewSize,
                                requested_size  = NewRequestedSize,
                                idle_workers    = NewIdleWorkers,
                                stats           = Stats#epool_stats{
                                    down_workers_count = Stats#epool_stats.down_workers_count + 1
                                }},

                            %% Return
                            {noreply, IdleWorkerDownState};

                        _ ->

                            %% UNKNOWN MONITOR REF

                            %% This monitor reference doesn't belong to us or no longer exist?
                            {noreply, State#epool_srv_pool_state{
                                stats           = Stats#epool_stats{
                                    down_unknown_count = Stats#epool_stats.down_unknown_count + 1
                                }}}

                    end

            end

    end;

handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns worker back into the pool
%% The worker we returned using this function is now orphan worker.
%% It was taken out from the busy workers pool,
%% all caller monitor references are demonitored at this stage.
private_worker_return(WorkerPid, WorkerMonitorRef, WorkerCreationTime,
                      NewBusyWorkers,
                      NewStats,
                      State = #epool_srv_pool_state{
    waiting_calls           = WaitingCalls,
    waiting_calls_module    = WaitingCallsModule
}) ->

    %% Check if we have waiting calls
    case WaitingCallsModule:take_head(WaitingCalls) of

        {#epool_waiting_call{
            caller_pid          = WaitingCallerPid,
            caller_ref          = WaitingCallerRef,
            caller_monitor_ref  = WaitingCallerMonitorRef}, WaitingCalls1} ->

            %% Waiting calls is NOT empty

            %% Get the busy worker module
            BusyWorkersModule = State#epool_srv_pool_state.busy_workers_module,

            %% Put the worker back into the busy workers with different monitor reference this time
            BusyWorkers2 = BusyWorkersModule:add(#epool_busy_worker{
                worker_pid           = WorkerPid,
                worker_monitor_ref   = WorkerMonitorRef,
                worker_creation_time = WorkerCreationTime,
                caller_pid           = WaitingCallerPid,
                caller_ref           = WaitingCallerRef,
                caller_monitor_ref   = WaitingCallerMonitorRef
            }, NewBusyWorkers),

            %% Give the worker to the caller
            gen_server:reply(WaitingCallerPid, {ok, WorkerPid}),

            %% Update the state and return
            State#epool_srv_pool_state{
                busy_workers  = BusyWorkers2,
                waiting_calls = WaitingCalls1,
                stats         = NewStats#epool_stats{
                    take_later_count  = NewStats#epool_stats.take_later_count + 1,
                    take_cancel_count = NewStats#epool_stats.take_cancel_count + 1
                }};

        _ ->

            %% Waiting calls is empty

            %% Get the pool strategy
            Strategy            = State#epool_srv_pool_state.strategy,

            %% Get the pool idle workers
            IdleWorkers         = State#epool_srv_pool_state.idle_workers,
            IdleWorkersModule   = State#epool_srv_pool_state.idle_workers_module,

            %% Put the worker back into idle workers using the strategy used in pool configuration
            IdleWorkers1 = IdleWorkersModule:add(Strategy, #epool_idle_worker{
                worker_pid              = WorkerPid,
                worker_monitor_ref      = WorkerMonitorRef,
                worker_creation_time    = WorkerCreationTime,
                worker_idle_time        = epool_utils:time_posix_seconds()
            }, IdleWorkers),

            %% Update the state and return
            State#epool_srv_pool_state{
                idle_workers = IdleWorkers1,
                busy_workers = NewBusyWorkers,
                stats        = NewStats#epool_stats{
                    take_cancel_count = NewStats#epool_stats.take_cancel_count + 1
                }}

    end.

%% @doc Start the culling mechanism if necessary

%% Don't alter culling started state when size is less than minimum poll size
private_start_culling(Size, MinSize, CullingStarted, _CullingInterval) when Size < MinSize -> CullingStarted;
%% Don't start culling if it is already started
private_start_culling(_Size, _MinSize, true, _CullingInterval) -> true;
%% Start culling mechanism
private_start_culling(_Size, _MinSize, false, CullingInterval) ->
    erlang:send_after(CullingInterval, self(), cull),
    true.
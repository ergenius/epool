%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Workers manager server
%%%
%%% Handle creation of pool workers asynchronously keeping the pool server more responsive during the workers creation process.
%%% Also handle retrying creation of the workers when workers start fail due to services downtime.
%%% @end
%%% Created : 14. Aug 2017 4:41 PM
%%%-------------------------------------------------------------------
-module(epool_srv_workers_manager).
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

-spec start_link(PoolConfig :: epool_config()) -> supervisor:start_ret().
start_link(PoolConfig = #epool_config{name = Name}) ->

    IdSrvWorkersManager = epool_utils:id_srv_workers_manager(Name),
    gen_server:start_link(IdSrvWorkersManager, ?MODULE, PoolConfig, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(#epool_config{
    name            = Name,
    worker_module   = WorkerModule,
    worker_args     = WorkerArgs
}) ->

    %% Build workers supervisor id
    IdSupWorkers        = epool_utils:id_sup_workers(Name),

    %% Build initial state
    State = #epool_srv_workers_manager_state{
        srv_pool        = Name,
        sup_workers     = IdSupWorkers,
        worker_module   = WorkerModule,
        worker_args     = WorkerArgs
    },

    {ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle stop
handle_call(stop, From, State) ->

    gen_server:reply(From, ok),
    {stop, normal, State};

%% Handle any other call
handle_call(_Msg, _From, State) -> {reply, error, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create new workers for the pool
handle_cast({create, HowMany}, State) ->

    %% Create the workers
    {noreply, private_create_workers(HowMany, 0, State)};

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Retry creating new workers for the pool
handle_info({retry, HowManyFailed, AttemptCount}, State) ->

    %% Retry creating the workers
    {noreply, private_create_workers(HowManyFailed, AttemptCount, State)};

%% Handle any other info
handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates the specified number of workers and put them into the pool

%% Stop retrying creating workers when retry limitations are reach
private_create_workers(_HowMany, AttemptCount,
                       State = #epool_srv_workers_manager_state{
                           retry_creating = Retry
                       }) when erlang:is_integer(Retry), AttemptCount > Retry -> State;

%% Continue creating workers
private_create_workers(HowMany, AttemptCount, State) ->

    %% Create the workers
    case private_create_workers(HowMany, State) of
        {ok, OkState} ->

            %% All workers were successfully added to the pool
            %% or the pool stopped adding new workers because it is full
            OkState;

        {start_fail, HowManyFailed, StartFailState} ->

            %% We failed starting `HowManyFailed` workers
            %% Check to see if configuration instruct us to try adding later the amount of workers
            %% we failed to start.

            %% You may find this strange. But it's not. Some workers may fail because downtime of some services they are
            %% using or trying to connect to during their initialisation process (like for example they fail connecting to
            %% a database server). The user of the pool may want the pool to try recreating the workers again and again
            %% until the services that prevent workers to start are restored.

            %% All existing pools implementations are not handling this situation and
            %% they usually end up unable to handle services downtime causing database or other services
            %% clients to stop working forever if the situation is not handled and no pool restart is issued.
            %% Github database drivers projects based on existing pool implementations are full with issues
            %% related to drivers not being able to recover from downtime's and those project are trying to fix
            %% this issue outside of the pools implementation when this could be very easily handled inside.
            private_create_workers_retry(HowManyFailed, AttemptCount, StartFailState)

    end.

%% @doc Creates the specified number of workers and put them into the pool
private_create_workers(0, State) -> {ok, State};
private_create_workers(HowMany, State = #epool_srv_workers_manager_state{sup_workers = SupWorkers}) ->

    %% Create the worker
    case supervisor:start_child(SupWorkers, []) of

        %% For a simple_one_for_one supervisor, when a child process start function returns ignore,
        %% the supervisor:start_child/2 function returns {ok, undefined} and no child is added to the supervisor.
        {ok, undefined} ->

            %% Failure starting worker
            %% Give caller the chance to retry
            {start_fail, HowMany, State};

        %% If the child process start function returns {ok, Child} or {ok, Child, Info},
        %% the child specification and pid are added to the supervisor and supervisor:start_child/2 returns the same value
        {ok, WorkerPid} -> private_create_workers_add(HowMany, WorkerPid, State);
        {ok, WorkerPid, _Info} -> private_create_workers_add(HowMany, WorkerPid, State);

        %% If the child process start function returns an error tuple or an erroneous value, or if it fails,
        %% the child specification is discarded, and the function returns {error,Error},
        %% where Error is a term containing information about the error and child specification.
        {error, _Error} ->

             %% Failure starting worker
             %% Give caller the chance to retry
             {start_fail, HowMany, State}

    end.

%% @doc Add the worker to the pool
private_create_workers_add(HowMany, WorkerPid, State = #epool_srv_workers_manager_state{
    srv_pool    = SrvPool,
    sup_workers = SupWorkers
}) ->

    %% Keep creation time
    CreationTime = epool_utils:time_posix_milliseconds(),

    %% Put the worker into the pool
    case gen_server:call(SrvPool, {add, WorkerPid, CreationTime}, infinity) of
        ok ->

            %% Worker was successfully added to the pool
            %% Iterate next and add another worker
            private_create_workers(HowMany - 1, SupWorkers, SrvPool);

        _ ->

            %% The pool is full or the worker creation was not requested
            %% by the pool server

            %% Terminate this worker
            supervisor:terminate_child(SupWorkers, WorkerPid),

            %% Stop trying to add more workers
            {ok, State}

    end.

%% @doc Retry creating workers in case of failure
private_create_workers_retry(_HowManyFailed, _AttemptCount,
                             State = #epool_srv_workers_manager_state{retry_creating = never}) -> State;
private_create_workers_retry(HowManyFailed, AttemptCount,
                             State = #epool_srv_workers_manager_state{retry_creating = forever}) ->

    %% Retry later
    erlang:send_after(State#epool_srv_workers_manager_state.retry_creating_interval, self(), {retry, HowManyFailed, AttemptCount + 1}),

    %% Return
    State;
private_create_workers_retry(HowManyFailed, AttemptCount,
                             State = #epool_srv_workers_manager_state{retry_creating = Retry}) when
    erlang:is_integer(Retry) ->

    %% Check retry limitations
    case AttemptCount < Retry of
        true ->

            %% Retry later
            erlang:send_after(State#epool_srv_workers_manager_state.retry_creating_interval, self(), {retry, HowManyFailed, AttemptCount + 1}),

            %% Return state
            State;

        false ->

            %% Return state
            State

    end.
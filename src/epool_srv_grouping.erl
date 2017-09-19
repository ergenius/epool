%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Pools grouping server
%%%
%%% This gen_server implements add/delete pools grouping operations and monitors pools added to the groups
%%% for termination and automatically removal from the groups.
%%%
%%% @end
%%% Created : 14. Sep 2017 9:07 AM
%%%-------------------------------------------------------------------
-module(epool_srv_grouping).
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

-spec start_link(Args :: proplists:proplist()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(Args) ->

    Name = application:get_env(epool, srv_grouping_name, ?EPOOL_DEFAULT_SRV_GROUPING_NAME),
    gen_server:start_link(Name, ?MODULE, Args, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->

    %% Create the public ets for storing pools by group
    %% This ETS can be used directly by the caller process to retrive the pools in a group (most used operation)
    %% without the need to call this server. This way this server will only handle
    %% add/delete pool grouping operations and will not be overflow by listing groups pools
    %% calls.
    ets:new(?EPOOL_ETS_GROUPING_POOLS_BY_GROUP, [bag, named_table, public]),

    %% Build initial state
    State = #epool_srv_grouping_state{
        monitors = []
    },

    {ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle add call
handle_call({add, GroupName, PoolName},
            {_CallerPid, _},
            State = #epool_srv_grouping_state{
                monitors = Monitors
            }) ->

    %% Always insert the pool into pools by groups ets
    ets:insert(?EPOOL_ETS_GROUPING_POOLS_BY_GROUP, {GroupName, PoolName}),

    %% Check to see if we need to monitor this pool (new pool, never added to any group before)
    %% Notice that we only create one monitor per pool the first time the pool is added to a group
    case private_monitors_exist_by_pool_name(PoolName, Monitors) of
        true ->

            %% Returns the state unaltered
            {reply, ok, State};

        false ->

            %% Pool is added for the first time...
            %% Monitor the pool server process (epool_srv_pool)
            NewMonitorRef = erlang:monitor(process, PoolName),

            %% Add the monitor to the monitors list
            {reply, ok, State#epool_srv_grouping_state{monitors = [{PoolName, NewMonitorRef} | Monitors]}}

    end;

%% Handle delete from all groups call
handle_call({delete, PoolName},
            {_CallerPid, _},
            State = #epool_srv_grouping_state{}) ->

    %% Delete the pool from all the groups
    %% Notice we don't delete any pool monitor so the same monitor can be used if
    %% we add the same pool to another group later or if the pool is already added to another groups
    ets:match_delete(?EPOOL_ETS_GROUPING_POOLS_BY_GROUP, {'_', PoolName}),

    %% Ok
    {reply, ok, State};

%% Handle delete from group call
handle_call({delete, GroupName, PoolName},
            {_CallerPid, _},
            State = #epool_srv_grouping_state{}) ->

    %% Delete the pool from the specified group
    %% Notice we don't delete any pool monitor so the same monitor can be used if
    %% we add the same pool to another group later or if the pool is already added to another groups
    ets:match_delete(?EPOOL_ETS_GROUPING_POOLS_BY_GROUP, {GroupName, PoolName}),

    %% Ok
    {reply, ok, State};

%% Handle any other call
handle_call(Msg, From, State) ->

    %% Log the unknown call
    ?EPOOL_LOG_WARNING_REPORT(epool_srv_grouping_unknown_call, [{msg, Msg}, {from, From}, {state, State}]),

    {reply, {error, unknown_call}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle workers or caller exit
handle_info({'DOWN', MonitorRef, process, _, _},
            State = #epool_srv_grouping_state{monitors = Monitors}) ->

    %% Notice there is no need to demonitor the monitor reference we receive
    %% because a process monitor is triggered only once,
    %% after that it is removed from both monitoring process and the monitored entity

    %% Remove the monitor
    case private_monitors_take_by_monitor_ref(Monitors, MonitorRef) of
        {{PoolName, MonitorRef}, NewMonitors} ->

            %% Remove the pool from all groups
            true = ets:match_delete(epool_ets_grouping, {'_', PoolName}),

            %% Return
            {noreply, State#epool_srv_grouping_state{monitors = NewMonitors}};

        _ ->

            %% The monitor reference was not found
            %% Ignore and return state unaltered
            {noreply, State}

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

%% @doc Returns true if the specified pool monitor exists
private_monitors_exist_by_pool_name(_PoolName, []) -> false;
private_monitors_exist_by_pool_name(PoolName, [{PoolName, _MonitorRef} | _T]) -> true;
private_monitors_exist_by_pool_name(PoolName, [{_PoolName, _MonitorRef} | T]) ->
    private_monitors_exist_by_pool_name(PoolName, T).

%% @doc Returns the monitor by monitor reference
private_monitors_get_by_monitor_ref(_MonitorRef, []) -> not_found;
private_monitors_get_by_monitor_ref(MonitorRef, [{PoolName, MonitorRef} | _T]) -> {PoolName, MonitorRef};
private_monitors_get_by_monitor_ref(MonitorRef, [{_PoolName, _MonitorRef} | T]) ->
    private_monitors_get_by_monitor_ref(MonitorRef, T).

%% @doc Delete the specified monitor by monitor reference
private_monitors_delete_by_monitor_ref(MonitorRef, [{_PoolName, MonitorRef} | T]) -> T;
private_monitors_delete_by_monitor_ref(MonitorRef, [H | T]) -> [H | private_monitors_delete_by_monitor_ref(MonitorRef, T)];
private_monitors_delete_by_monitor_ref(_MonitorRef, []) -> [].

%% @doc Returns and delete the specified monitor by monitor reference
private_monitors_take_by_monitor_ref(MonitorRef, Monitors) ->

    case private_monitors_get_by_monitor_ref(MonitorRef, Monitors) of
        not_found -> not_found;
        Monitor -> {Monitor, private_monitors_delete_by_monitor_ref(MonitorRef, Monitors)}
    end.

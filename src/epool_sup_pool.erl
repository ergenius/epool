%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(epool_sup_pool).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

-include_lib("epool/include/epool.hrl").

%% supervisor exports
-export([start_link/1]).
-export([init/1]).
-export([upgrade/0]).

%% @doc Creates a supervisor process as part of a supervision tree.
start_link(PoolConfig = #epool_config{name = Name}) ->

    IdSupPool = epool_utils:id_sup_pool(Name),
    supervisor:start_link(IdSupPool, ?MODULE, PoolConfig).

%% @doc Returns supervisor flags and child specifications.
init(EpoolConfig = #epool_config{
    name                         = Name,
    sup_pool_restart_intensity   = RestartIntensity,
    sup_pool_restart_period      = RestartPeriod,
    sup_workers_shutdown         = SupWorkersShutdown,
    srv_workers_manager_shutdown = SrvWorkersManagerShutdown,
    srv_pool_shutdown            = SrvPoolShutdown
}) ->

    %% Build id for the workers supervisor
    IdSupWorkers        = epool_utils:id_sup_workers(Name),

    %% Build id for the workers manager server
    IdSrvWorkersManager = epool_utils:id_srv_workers_manager(Name),

    %% Returns supervisor flags and child specifications
    %% for two children: one worker supervisor and one pool server
    {ok, {

        %% one_for_all - If a child process terminates, all other child processes are terminated, and then all child processes,
        %% including the terminated one, are restarted.
        {one_for_all, RestartIntensity, RestartPeriod},
        [

            %% Pool workers supervisor
            {
                IdSupWorkers,
                {epool_sup_workers, start_link, [EpoolConfig]},

                %% A transient child process is restarted only if it terminates abnormally, that is,
                %% with an exit reason other than normal, shutdown, or {shutdown,Term}.
                transient,
                SupWorkersShutdown,
                supervisor,
                [epool_sup_workers]
            },

            %% Pool workers manager server
            {
                IdSrvWorkersManager,
                {epool_srv_workers_manager, start_link, [EpoolConfig]},

                %% A transient child process is restarted only if it terminates abnormally, that is,
                %% with an exit reason other than normal, shutdown, or {shutdown,Term}.
                transient,
                SrvWorkersManagerShutdown,
                worker,
                [epool_srv_workers_manager]
            },

            %% Pool server
            {
                Name,
                {epool_srv_pool, start_link, [EpoolConfig]},

                %% A transient child process is restarted only if it terminates abnormally, that is,
                %% with an exit reason other than normal, shutdown, or {shutdown,Term}.
                transient,
                SrvPoolShutdown,
                worker,
                [epool_srv_pool]
            }

        ]
    }}.

%% @spec upgrade() -> ok
%% @doc Handle the upgrade process.
upgrade() -> ok.

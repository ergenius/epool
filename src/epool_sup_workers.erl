%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2017 4:41 PM
%%%-------------------------------------------------------------------
-module(epool_sup_workers).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

-include_lib("epool/include/epool.hrl").

%% supervisor exports
-export([start_link/1]).
-export([init/1]).
-export([upgrade/0]).

%% @doc Creates a supervisor process as part of a supervision tree.
start_link(PoolConfig = #epool_config{name = Name}) ->

    IdSupWorkers = epool_utils:id_sup_workers(Name),
    supervisor:start_link(IdSupWorkers, ?MODULE, PoolConfig).

%% @doc Returns supervisor flags and child specifications.
init(#epool_config{
    worker_module                 = WorkerModule,
    worker_args                   = WorkerArgs,
    sup_workers_shutdown          = Shutdown,
    sup_workers_restart_intensity = RestartIntensity,
    sup_workers_restart_period    = RestartPeriod
}) ->

    %% Returns supervisor flags and child specifications
    %%
    %% A supervisor with restart strategy simple_one_for_one is a simplified one_for_one supervisor,
    %% where all child processes are dynamically added instances of the same process.
    %%
    %% Notice when started, the supervisor does not start any child processes.
    %% Instead, all child processes are added dynamically by calling: supervisor:start_child(Sup, List).
    %%
    %% A child under a simple_one_for_one supervisor can be terminated with the following:
    %% supervisor:terminate_child(Sup, Pid)
    %% Sup is the pid, or name, of the supervisor and Pid is the pid of the child.
    %%
    %% Because a simple_one_for_one supervisor can have many children, it shuts them all down asynchronously.
    %% This means that the children will do their cleanup in parallel and therefore the order in which they are stopped is not defined.
    {ok, {
        {simple_one_for_one, RestartIntensity, RestartPeriod},
        [
            {WorkerModule, {WorkerModule, start_link, [WorkerArgs]},
             temporary,
             Shutdown,
             worker,
             [WorkerModule]}
        ]
    }}.

%% @spec upgrade() -> ok
%% @doc Handle the upgrade process.
upgrade() -> ok.

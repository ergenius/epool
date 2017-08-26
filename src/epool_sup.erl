%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(epool_sup).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

-include_lib("epool/include/epool.hrl").

%% supervisor exports
-export([start_link/2]).
-export([init/1]).
-export([upgrade/0]).

%% @doc Creates a supervisor process as part of a supervision tree.
start_link(SupName, Args) ->

    supervisor:start_link(SupName, ?MODULE, Args).

%% @doc Returns supervisor flags and child specifications.
init(Args) ->

    %% Get epool main supervisor configuration parameters
    RestartIntensity    = epool_utils:config_get_value(sup_restart_intensity, Args, ?EPOOL_DEFAULT_SUP_RESTART_INTENSITY),
    RestartPeriod       = epool_utils:config_get_value(sup_restart_period, Args, ?EPOOL_DEFAULT_SUP_RESTART_INTENSITY),
    PoolsConfig         = epool_utils:config_get_value(pools, Args, []),
    GroupingModule      = epool_utils:config_get_value(grouping_module, Args, ?EPOOL_DEFAULT_GROUPING_MODULE),

    %% Iterate pools config list and build pools child specifications
    PoolsSpecs   = [epool_utils:pool_config_to_child_specs(PoolConfig) || PoolConfig <- PoolsConfig],

    %% Init default grouping module and add any grouping child to the supervisor children specifications
    ChildSpecs = case GroupingModule:init() of
                     {ok, GroupingModuleChildSpecs} -> [GroupingModuleChildSpecs | PoolsSpecs];
                     ok -> PoolsSpecs
                 end,

    %% Returns supervisor flags and child specifications
    {ok, {{one_for_one, RestartIntensity, RestartPeriod}, ChildSpecs}}.

%% @spec upgrade() -> ok
%% @doc Handle the upgrade process.
upgrade() -> ok.

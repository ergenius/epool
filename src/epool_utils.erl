%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%% Created : 12. Aug 2017 7:20 PM
%%%-------------------------------------------------------------------
-module(epool_utils).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([time_posix_seconds/0]).
-export([time_posix_milliseconds/0]).
-export([time_posix_microseconds/0]).
-export([time_posix_nanoseconds/0]).

-export([id_sup_pool/1]).
-export([id_sup_pool_to_pool_name/1]).
-export([id_srv_workers_manager/1]).
-export([id_sup_workers/1]).

-export([time_spec_to_milliseconds/1]).
-export([time_spec_to_seconds/1]).

-export([config_get_value/3]).

-export([pool_config_to_child_specs/1]).
-export([pool_config_from_proplist/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the seconds
time_posix_seconds() -> erlang:system_time(second).

%% @doc Returns the milliseconds
time_posix_milliseconds() -> erlang:system_time(millisecond).

%% @doc Returns the microseconds
time_posix_microseconds() -> erlang:system_time(microsecond).

%% @doc Returns the nanosecond
time_posix_nanoseconds() -> erlang:system_time(nanosecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the supervisor id of the specified pool
id_sup_pool(PoolName) -> erlang:list_to_atom("epool_sup_pool_" ++ erlang:atom_to_list(PoolName)).

%% @doc Returns the workers supervisor id of the specified pool
id_sup_workers(PoolName) -> erlang:list_to_atom("epool_sup_workers_" ++ erlang:atom_to_list(PoolName)).

%% @doc Returns the workers manager server id of the specified pool
id_srv_workers_manager(PoolName) -> erlang:list_to_atom("epool_srv_workers_manager_" ++ erlang:atom_to_list(PoolName)).

%% @doc Extract the pool name for the pool supervisor id
id_sup_pool_to_pool_name(Id) ->

    case erlang:atom_to_list(Id) of
        ["" | PoolName] -> {ok, erlang:list_to_atom(PoolName)};
        _ -> {error, invalid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert time specification TimeSpec to milliseconds.
time_spec_to_milliseconds(TimeUnit) when erlang:is_integer(TimeUnit) -> TimeUnit;
time_spec_to_milliseconds({TimeUnit, minute}) -> TimeUnit * 60000;
time_spec_to_milliseconds({TimeUnit, second}) -> TimeUnit * 1000;
time_spec_to_milliseconds({TimeUnit, millisecond}) -> TimeUnit;
time_spec_to_milliseconds({TimeUnit, microsecond}) -> TimeUnit  div 1000.

%% @doc Convert time specification TimeSpec to seconds.
time_spec_to_seconds(TimeUnit) when erlang:is_integer(TimeUnit) -> TimeUnit;
time_spec_to_seconds({TimeUnit, minute}) -> TimeUnit * 60;
time_spec_to_seconds({TimeUnit, second}) -> TimeUnit;
time_spec_to_seconds({TimeUnit, millisecond}) -> TimeUnit div 1000;
time_spec_to_seconds({TimeUnit, microsecond}) -> TimeUnit  div 1000000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified configuration option from the specified proplist.
config_get_value(Key, PoolConfig, Default) when erlang:is_list(PoolConfig) ->
    proplists:get_value(Key, PoolConfig, application:get_env(epool, Key, Default)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pool config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert pool options to epool_sup supervisor child specs
pool_config_to_child_specs(PoolConfig) when erlang:is_list(PoolConfig) ->
    pool_config_to_child_specs(pool_config_from_proplist(PoolConfig));

%% Generate child specs for poolboy supervision type
pool_config_to_child_specs(PoolConfig = #epool_config{
    name                = Name,
    sup_pool_shutdown   = Shutdown
}) ->

    %% Build pool supervisor id
    IdSupPool = epool_utils:id_sup_pool(Name),

    %% We use legacy specs for now to remain compatible with older erlang versions we still want to support
    {IdSupPool, {epool_sup_pool, start_link, [PoolConfig]}, transient, Shutdown, supervisor, [epool_sup_pool]}.

%% @doc Create a epool_options record from the specified proplist
-spec pool_config_from_proplist(PoolConfig) -> epool_config() when
    PoolConfig :: [{atom(), term()}].
pool_config_from_proplist(PoolConfig) when erlang:is_list(PoolConfig) ->

    %% Extract config parameters from proplist and creates the epool config record
    #epool_config{
        name  = proplists:get_value(name, PoolConfig, undefined)
    }.



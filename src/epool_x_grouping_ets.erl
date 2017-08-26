%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2017 10:34 AM
%%%-------------------------------------------------------------------
-module(epool_x_grouping_ets).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([init/0]).
-export([add_pool/2]).
-export([delete_pool/2]).
-export([list_pools/1]).

%% @doc Init this grouping implementation
-spec init() -> ok | {ok, GroupingChildSpecs} when
    GroupingChildSpecs :: supervisor:child_spec().
init() ->

    ets:new(epool_ets_grouping, [bag, named_table, public]),
    ok.

%% @doc Add the specified pool to the specified group
-spec add_pool(GroupName, PoolName) -> true when
    GroupName   :: atom(),
    PoolName    :: epool_name().
add_pool(GroupName, PoolName) ->

    ets:insert(epool_ets_grouping, {GroupName, PoolName}).

%% @doc Delete the specified pool from specified group
-spec delete_pool(GroupName, PoolName) -> true when
    GroupName   :: atom(),
    PoolName    :: epool_name().
delete_pool(GroupName, PoolName) ->

    ets:match_delete(epool_ets_grouping, {GroupName, PoolName}).

%% @doc Returns a list with all pools into the specified group
-spec list_pools(GroupName) -> [PoolName] when
    GroupName   :: atom(),
    PoolName    :: epool_name().
list_pools(GroupName) ->

    ets:match(epool_ets_grouping, {GroupName, '$1'}).

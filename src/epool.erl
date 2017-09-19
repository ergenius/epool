%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% This module contains Epool API
%%% @end
%%%-------------------------------------------------------------------
-module(epool).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

-export([create/1]).
-export([delete/1]).
-export([exists/1]).
-export([status/1]).
-export([list/0]).

-export([take/1, take/3]).
-export([take_from_pools/1, take_from_pools/3, take_from_pools/4]).
-export([take_from_group/1, take_from_group/3, take_from_group/4]).
-export([release/2]).
-export([transaction/2, transaction/4]).

-export([grouping_add_pool/2]).
-export([grouping_remove_pool/1, grouping_remove_pool/2]).
-export([grouping_list_pools/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pool CRUD operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates a new pool
-spec create(PoolConfig) -> {ok, Pid} | {error, Reason} when
    PoolConfig	:: epool_config(),
    Pid        	:: pid(),
    Reason     	:: term().
create(PoolConfig) ->

    %% Get main supervisor reference
    MainSupRef = application:get_env(epool, sup_ref, ?EPOOL_DEFAULT_SUP_REF),

    %% Build pool supervisor child specifications
    ChildSpec   = epool_utils:pool_config_to_child_specs(PoolConfig),

    %% Starts the pool supervisor child
    supervisor:start_child(MainSupRef, ChildSpec).

%% @doc Delete the specified pool and terminate all workers
-spec delete(PoolName) -> ok | {error, Reason} when
    PoolName	:: epool_name(),
    Reason     	:: term().
delete(PoolName) ->

    %% Get main supervisor reference
    MainSupRef = application:get_env(epool, sup_ref, ?EPOOL_DEFAULT_SUP_REF),

    %% Get the supervisor id of the specified pool
    IdSupPool   = epool_utils:id_sup_pool(PoolName),

    %% Tells main supervisor to terminate the pool supervisor child
    case supervisor:terminate_child(MainSupRef, IdSupPool) of
        ok ->

            %% Tells main supervisor to delete the child specification identified by Id.
            %% The corresponding child process must not be running. That's why we use first terminate_child/2 to terminate it.
            %% If successful, the function returns ok. If the child specification identified by Id exists but the corresponding
            %% child process is running or is about to be restarted, the function returns {error,running} or {error,restarting}, respectively.
            %% If the child specification identified by Id does not exist, the function returns {error, not_found}.
            supervisor:delete_child(MainSupRef, IdSupPool);

        Error -> Error
    end.

%% @doc Check if the specified pool exists
-spec exists(PoolName) -> boolean() when
    PoolName		:: epool_name().
exists(PoolName) ->

    %% Get main supervisor reference
    MainSupRef = application:get_env(epool, sup_ref, ?EPOOL_DEFAULT_SUP_REF),

    %% Returns a newly created list with information about all child specifications and child processes belonging to supervisor SupRef.
    %% Notice that calling this function when supervising many children under low memory conditions can cause an out of memory exception.
    ChildSpecs  = supervisor:which_children(MainSupRef),

    %% Get the supervisor id of the specified pool
    IdSupPool   = epool_utils:id_sup_pool(PoolName),

    %% Check if the specified supervisor pool exists
    lists:keymember(IdSupPool, 1, ChildSpecs).

%% @doc Returns the status of the specified pool
-spec status(PoolName) -> {ok, Status} | {error, Reason} when
    PoolName    :: epool_name(),
    Status      :: epool_status(),
    Reason      :: term().
status(PoolName) ->

    try
        gen_server:call(PoolName, status)
    catch
        Type:Exception -> {error, {exception, Type, Exception}}
    end.

%% @doc List all pools
-spec list() -> PoolsList when PoolsList :: [epool_name()].
list() ->

    %% Get main supervisor reference
    MainSupRef = application:get_env(epool, sup_ref, ?EPOOL_DEFAULT_SUP_REF),

    ChildSpecs = supervisor:which_children(MainSupRef),
    list(ChildSpecs, []).

list([[{Id, _Child, supervisor, _Modules}]|T], Acum) ->

    %% Get the pool name from the pool supervisor id
    case epool_utils:id_sup_pool_to_pool_name(Id) of
        {ok, PoolName} -> list(T, [PoolName | Acum]);
        _ ->
            %% The name doesn't correspond to the pattern we use for a pool supervisor
            %% ignore it
            list(T, Acum)
    end;
list([], Acum) -> Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%===========================================
%% take
%%===========================================

%% @doc Take a worker from the specified pool and return it to the caller with infinity timeout.
%% The caller will wait indefinitely until the pool server returns. If no worker is available, the call will be stored
%% in the pool waiting calls queue and the pool will give a worker later when the worker will become available.
-spec take(PoolName) -> {ok, Pid} | {error, Reason} when
    PoolName    :: epool_name(),
    Pid        	:: pid(),
    Reason     	:: term().

take(PoolName) -> take(PoolName, true, inifinity).

%% @doc Take a worker from the specified pool and return it to the caller with wait and timeout.
%%
%% Wait is a boolean `true` or `false`. When `Wait` is true the caller will wait until the pool server returns
%% or until the call timeout. If no worker is available, the call will be stored in the pool waiting calls queue
%% and the pool will give a worker later when a worker will become available for the call.
%% If `Wait` is false and no worker is available, the function will return {error, no_available_worker} tuple as fast as possible
%% without storing the call into the pooler waiting calls queue.
%%
%% Timeout is an integer greater than zero that specifies how many milliseconds to wait for a reply from the pool server.
%%
%% If no reply is received within the specified time, the function returns {error, Reason}.
%%
%% If the the server is just late with the reply, it can arrive at any time later into the message queue of the caller.
%% The caller must in this case be prepared for this and discard any such garbage messages that are
%% two element tuples with a reference as the first element.
-spec take(PoolName, Wait, Timeout) -> {ok, Pid} | {error, Reason} when
    PoolName	:: epool_name(),
    Wait        :: boolean(),
    Timeout     :: infinity | non_neg_integer(),
    Pid        	:: pid(),
    Reason     	:: term().

take(PoolName, Wait, Timeout) ->

    %% Make a client reference used to trace the call into the pool if we want to cancel the call
    %% due to a call exception using cancel_take cast
    ClientReference = make_ref(),
    try

        gen_server:call(PoolName, {take, Wait, ClientReference}, Timeout)

    catch

        Type:Exception ->

            %% Notice that the warning report is generated only when epool_debug preprocessor directive is enabled
            %% in order not to flood any production applications with warning reports messages
            %%
            %% Also notice that not handling error_logger warning reports may not stop the info propagation to the error logger
            %% handler so the fastest method to filter out those reports is using a preprocessor directive not a custom error logger handler
            ?EPOOL_LOG_WARNING_REPORT(take_exception, [{pool, PoolName}, {timeout, Timeout}, {exception, {Type, Exception}}]),

            %% Notify the pool server about this situation
            %% (Sends an asynchronous request to the ServerRef of the gen_server process and returns ok immediately)
            gen_server:cast(PoolName, {cancel_take, ClientReference}),

            %% Error
            {error, {exception, Type, Exception}}

    end.

%%===========================================
%% take_from_pools
%%===========================================

%% @doc Take a worker from the specified list of pools with infinity timeout
%%
%% This function make sense to be used when some pools from the list are empty (contains 0 workers) and other are not.
%% Another use case scenario is when you want to handle any error in taking a pool member, errors that may
%% be returned either because the specified pool no longer exists.
%%
%% Otherwise it is more appropriate to use take_from_pools/2 with an integer timeout since you will be wait forever
%% for the first pool in the list to return an worker and you will iterate the list only if first attempt fail because of errors.
%%
%% However this function is available for you to meet unusual requirements like the ones presented before
%% if your application logic may allow such situations to occur.
-spec take_from_pools(PoolNames) -> {ok, PoolName, Pid} | {error, Reason} when
    PoolNames   :: [epool_name()],
    PoolName	:: epool_name(),
    Pid        	:: pid(),
    Reason     	:: term().

take_from_pools(PoolNames) -> take_from_pools(PoolNames, true, infinity).

%% @doc Take a worker from the specified list of pools with timeout and an optional increment value for each attempt.
%%
%% Timeout can be an integer specifying the timeout value in milliseconds or a tuple {Timeout, TimeoutIncrement}.
%%
%% Please notice the timeout is not the timeout for the entire function call but a separate timeout for
%% getting a worker from every pool into the list. For example if there are 5 pools into the list,
%% timeout is set to 10000 milliseconds and all the pools take calls fail because of timeout
%% by calling this function you will timeout after at least 5 * 10000 = 50000 milliseconds.
%%
%% If the timeout is specified using a tuple containing timeout and timeout increment value,
%% the timeout increment value will be added to the timeout value for every new attempt to get a client from a
%% new pool into the pool names list. If you want to increase the timeout use a positive integer value.
%% If you want to decrease the timeout for every new attempt use a negative integer value.
-spec take_from_pools(PoolNames, Wait, Timeout) -> {ok, PoolName, Pid} | {error, Reason} when
    PoolNames   :: [epool_name()],
    Wait        :: boolean(),
    Timeout     :: infinity | non_neg_integer() | {non_neg_integer(), integer()},
    PoolName	:: epool_name(),
    Pid        	:: pid(),
    Reason     	:: term().

take_from_pools([PoolName|T], Wait, {Timeout, TimeoutIncrement}) ->
    case take(PoolName, Wait, Timeout) of
        {ok, Pid} -> {ok, PoolName, Pid};
        _ -> take_from_pools(T, Wait, {Timeout + TimeoutIncrement, TimeoutIncrement})
    end;
take_from_pools([PoolName|T], Wait, Timeout) ->
    case take(PoolName, Wait, Timeout) of
        {ok, Pid} -> {ok, PoolName, Pid};
        _ -> take_from_pools(T, Wait, Timeout)
    end;
take_from_pools([], _Wait, _Timeout) -> {error, no_available_worker}.

%% @doc Take a worker from the specified list of pools with total timeout and a specific timeout for each attempt.
%%
%% The total timeout is used for the entire function call. The attempt timeout is used for every attempt of getting a client
%% from a different pool.
-spec take_from_pools(PoolNames, Wait, AttemptTimeout, TotalTimeout) -> {ok, PoolName, Pid} | {error, Reason} when
    PoolNames       :: [epool_name()],
    Wait            :: boolean(),
    AttemptTimeout  :: infinity | non_neg_integer() | {non_neg_integer(), integer()},
    TotalTimeout    :: non_neg_integer(),
    PoolName	    :: epool_name(),
    Pid        	    :: pid(),
    Reason     	    :: term().

take_from_pools(PoolNames, Wait, AttemptTimeout, TotalTimeout) ->

    Reference = erlang:make_ref(),
    spawn(fun() ->
        Result = take_from_pools(PoolNames, Wait, AttemptTimeout),
        self() ! {Reference, Result}
                 end),
    receive
        {Reference, Result} -> Result
    after
        TotalTimeout -> {error, timeout}
    end.

%%===========================================
%% take_from_group
%%===========================================

%% @doc Take a worker from the specified pools group and return it to the caller with infinity timeout.
-spec take_from_group(GroupName) -> {ok, Pid} | {error, Reason} when
    GroupName   :: epool_group_name(),
    Pid        	:: pid(),
    Reason     	:: term().

take_from_group(GroupName) -> take_from_group(GroupName, true, inifinity).

%% @doc Take a worker from the specified pools group and return it to the caller with timeout.
-spec take_from_group(GroupName, Wait, Timeout) -> {ok, Pid} | {error, Reason} when
    GroupName	:: epool_group_name(),
    Wait        :: boolean(),
    Timeout     :: infinity | non_neg_integer() | {non_neg_integer(), integer()},
    Pid        	:: pid(),
    Reason     	:: term().

take_from_group(GroupName, Wait, Timeout) ->

    %% Get all pools into the specified group
    Pools = grouping_list_pools(GroupName),

    %% Take a worker from the specified list of pools
    take_from_pools(Pools, Wait, Timeout).

%% @doc Take a worker from the specified pools group with total timeout and a specific timeout for each attempt.
%%
%% The total timeout is used for the entire function call. The attempt timeout is used for every attempt of getting a client
%% from a different pool.
-spec take_from_group(GroupName, Wait, AttemptTimeout, TotalTimeout) -> {ok, PoolName, Pid} | {error, Reason} when
    GroupName	    :: epool_group_name(),
    Wait            :: boolean(),
    AttemptTimeout  :: infinity | non_neg_integer() | {non_neg_integer(), integer()},
    TotalTimeout    :: non_neg_integer(),
    PoolName	    :: epool_name(),
    Pid        	    :: pid(),
    Reason     	    :: term().

take_from_group(GroupName, Wait, AttemptTimeout, TotalTimeout) ->

    %% Get all pools into the specified group
    Pools = grouping_list_pools(GroupName),

    %% Take a worker from the specified list of pools
    take_from_pools(Pools, Wait, AttemptTimeout, TotalTimeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% release
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Release the specified worker into the specified pool.
-spec release(PoolName, PoolWorker) -> ok when
    PoolName	:: epool_name(),
    PoolWorker 	:: pid().

release(PoolName, PoolWorker) ->

    %% Notice that we cast because we don't want the caller to wait until the worker is released
    %% (Sends an asynchronous request to the ServerRef of the gen_server process and returns ok immediately)
    gen_server:cast(PoolName, {release, PoolWorker}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% transaction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Executes the functional object Fun with argument Worker as a transaction.
%%
%% If the transaction can not be executed because failure of taking a worker from the specified pool
%% the entire transaction is terminated and the function transaction/2 returns the tuple {aborted, {error, ErrorReason}}.
%%
%% If something goes wrong inside the transaction as a result of a error,
%% the entire transaction is terminated and the function transaction/2 returns the tuple {aborted, {exception, ExceptionType, ExceptionPattern}}.
-spec transaction(PoolName, Fun)  -> {ok, Result} | {aborted, Reason} when
    PoolName	:: epool_name(),
    Fun         :: fun((pid()) -> any()),
    Result      :: term(),
    Reason      :: {exception, term(), term()} | term().
transaction(PoolName, Fun) -> transaction(PoolName, Fun, true, infinity).

%% @doc Executes the functional object Fun with argument Worker as a transaction.
%%
%% If the transaction can not be executed because failure of taking a worker from the specified pool
%% the entire transaction is terminated and the function transaction/3 returns the tuple {aborted, {error, ErrorReason}}.
%%
%% If something goes wrong inside the transaction as a result of a error,
%% the entire transaction is terminated and the function transaction/3 returns the tuple {aborted, {exception, ExceptionType, ExceptionPattern}}.
-spec transaction(PoolName, Fun, Wait, Timeout) -> {ok, Result} | {aborted, Reason} when
    PoolName	:: epool_name(),
    Fun         :: fun((pid()) -> any()),
    Wait        :: boolean(),
    Timeout     :: infinity | non_neg_integer(),
    Result      :: term(),
    Reason      :: {exception, term(), term()} | term().
transaction(PoolName, Fun, Wait, Timeout) ->

    case take(PoolName, Wait, Timeout) of
        {ok, Worker} ->
            try
                Fun(Worker)
            catch
                Type:Exception ->
                    ok = release(PoolName, Worker),
                    {aborted, {exception, Type, Exception}}
            end;
        Error -> {aborted, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grouping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified pool to the specified group or to a list of groups.
-spec grouping_add_pool(GroupName, PoolName) -> ok when
    GroupName   :: atom() | [atom()],
    PoolName    :: epool_name().
grouping_add_pool([], _PoolName) -> ok;
grouping_add_pool([H|T], PoolName) when erlang:is_atom(H) ->
    ok = grouping_add_pool(H, PoolName),
    grouping_add_pool(T, PoolName);
grouping_add_pool(GroupName, PoolName) when erlang:is_atom(GroupName) ->

    %% Get grouping server reference
    GroupingServerRef = application:get_env(epool, srv_grouping_ref, ?EPOOL_DEFAULT_SRV_GROUPING_REF),

    %% Call the grouping server
    gen_server:call(GroupingServerRef, {add, GroupName, PoolName}, infinity).

%% @doc Remove the specified pool from all the group it was added to
-spec grouping_remove_pool(PoolName) -> ok when
    PoolName    :: epool_name().
grouping_remove_pool(PoolName) ->

    %% Get grouping server reference
    GroupingServerRef = application:get_env(epool, srv_grouping_ref, ?EPOOL_DEFAULT_SRV_GROUPING_REF),

    %% Call the grouping server
    gen_server:call(GroupingServerRef, {delete, PoolName}, infinity).

%% @doc Remove the specified pool from specified group
-spec grouping_remove_pool(GroupName, PoolName) -> true when
    GroupName   :: atom(),
    PoolName    :: epool_name().
grouping_remove_pool(GroupName, PoolName) ->

    %% Get grouping server reference
    GroupingServerRef = application:get_env(epool, srv_grouping_ref, ?EPOOL_DEFAULT_SRV_GROUPING_REF),

    %% Call the grouping server
    gen_server:call(GroupingServerRef, {delete, GroupName, PoolName}, infinity).

%% @doc Returns a list with all pools into the specified group
-spec grouping_list_pools(GroupName) -> [PoolName] when
    GroupName   :: atom(),
    PoolName    :: epool_name().
grouping_list_pools(GroupName) ->

    %% We operate directly on the grouping ETS without unnecessary calling
    %% the grouping server.
    ets:match(?EPOOL_ETS_GROUPING_POOLS_BY_GROUP, {GroupName, '$1'}).

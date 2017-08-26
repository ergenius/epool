%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Implements operations for waiting calls based on erlang lists.
%%% @end
%%% Created : 11. Aug 2017 4:09 PM
%%%-------------------------------------------------------------------
-module(epool_x_waiting_calls_list).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API
-export([new/0]).

-export([add/2, add/3]).

-export([take_head/1]).
-export([take_by_caller_ref/2]).
-export([take_by_caller_monitor_ref/2]).

%% Type definition for waiting calls list
-type epool_waiting_calls() :: [epool_waiting_call()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates and return a new, empty waiting calls list
new() -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified waiting call to the front of waiting calls list
%% Attach the element to the beginning of the list which is faster
-spec add(WaitingCall, WaitingCalls) -> WaitingCalls2 when
    WaitingCall      :: epool_waiting_call(),
    WaitingCalls     :: epool_waiting_calls(),
    WaitingCalls2    :: epool_waiting_calls().

add(WaitingCall, WaitingCalls) -> [WaitingCall | WaitingCalls].

%% @doc Add the specified waiting call to the waiting calls list with custom strategy
-spec add(Strategy, WaitingCall, WaitingCalls) -> WaitingCalls2 when
    Strategy        :: lifo | fifo | random,
    WaitingCall      :: epool_waiting_call(),
    WaitingCalls     :: epool_waiting_calls(),
    WaitingCalls2    :: epool_waiting_calls().

add(lifo, WaitingCall, WaitingCalls) -> [WaitingCall | WaitingCalls];
add(fifo, WaitingCall, WaitingCalls) -> WaitingCalls ++ [WaitingCall];
add(random, WaitingCall, WaitingCalls) ->
    case (trunc(rand:uniform() * 2) + 1) of
        1 -> add(lifo, WaitingCall, WaitingCalls);
        2 -> add(fifo, WaitingCall, WaitingCalls)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take the waiting call from the head of the waiting calls if any
-spec take_head(WaitingCalls) -> {WaitingCall, WaitingCalls2} | empty when
    WaitingCalls     :: epool_waiting_calls(),
    WaitingCall      :: epool_waiting_call(),
    WaitingCalls2    :: epool_waiting_calls().

take_head([WaitingCall | WaitingCalls2]) -> {WaitingCall, WaitingCalls2};
take_head([]) -> empty.

%% @doc Take the specified waiting call from the waiting calls by caller reference
-spec take_by_caller_ref(CallerRef, WaitingCalls) -> WaitingCall | not_found when
    CallerRef    :: reference(),
    WaitingCalls :: epool_waiting_calls(),
    WaitingCall  :: epool_waiting_call().

take_by_caller_ref(CallerRef, WaitingCalls) ->

    case get_by_caller_ref(CallerRef, WaitingCalls) of
        not_found -> not_found;
        WaitingCall -> {WaitingCall, delete_by_caller_ref(CallerRef, WaitingCalls)}
    end.

%% @doc Take the specified waiting call from the waiting calls by caller monitor reference
-spec take_by_caller_monitor_ref(CallMonitorRef, WaitingCalls) -> WaitingCall | not_found when
    CallMonitorRef    :: reference(),
    WaitingCalls         :: epool_waiting_calls(),
    WaitingCall          :: epool_waiting_call().

take_by_caller_monitor_ref(CallMonitorRef, WaitingCalls) ->

    case get_by_caller_monitor_ref(CallMonitorRef, WaitingCalls) of
        not_found -> not_found;
        BusyCall -> {BusyCall, delete_by_caller_monitor_ref(CallMonitorRef, WaitingCalls)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the waiting call with caller monitor reference `CallerRef` from waiting calls `WaitingCalls`.
%% Returns the idle waiting call if found or `not_found` otherwise.
-spec get_by_caller_ref(CallerRef, WaitingCalls) -> WaitingCall | not_found when
    CallerRef    :: reference(),
    WaitingCalls        :: epool_waiting_calls(),
    WaitingCall         :: epool_waiting_call().

get_by_caller_ref(CallerRef, [WaitingCall = #epool_waiting_call{caller_ref = CallerRef} | _]) -> WaitingCall;
get_by_caller_ref(CallerRef, [_ | T]) -> get_by_caller_ref(CallerRef, T);
get_by_caller_ref(_CallerRef, []) -> not_found.

%% @doc Returns the waiting call with caller monitor reference `CallerRef` from waiting calls `WaitingCalls`.
%% Returns the idle waiting call if found or `not_found` otherwise.
-spec get_by_caller_monitor_ref(CallerMonitorRef, WaitingCalls) -> WaitingCall | not_found when
    CallerMonitorRef    :: reference(),
    WaitingCalls        :: epool_waiting_calls(),
    WaitingCall         :: epool_waiting_call().

get_by_caller_monitor_ref(CallerMonitorRef, [WaitingCall = #epool_waiting_call{caller_monitor_ref = CallerMonitorRef} | _]) -> WaitingCall;
get_by_caller_monitor_ref(CallerMonitorRef, [_ | T]) -> get_by_caller_monitor_ref(CallerMonitorRef, T);
get_by_caller_monitor_ref(_CallerMonitorRef, []) -> not_found.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete the waiting call with caller monitor reference `CallerRef` from waiting calls `WaitingCalls`.
-spec delete_by_caller_ref(CallerRef, WaitingCalls) -> WaitingCalls2 when
    CallerRef            :: reference(),
    WaitingCalls         :: epool_waiting_calls(),
    WaitingCalls2        :: epool_waiting_calls().

delete_by_caller_ref(CallerRef, [#epool_waiting_call{caller_monitor_ref = CallerRef} | T]) -> T;
delete_by_caller_ref(CallerRef, [H | T]) -> [H | delete_by_caller_ref(CallerRef, T)];
delete_by_caller_ref(_CallerRef, []) -> [].

%% @doc Delete the waiting call with caller monitor reference `CallerMonitorRef` from waiting calls `WaitingCalls`.
-spec delete_by_caller_monitor_ref(CallerMonitorRef, WaitingCalls) -> WaitingCalls2 when
    CallerMonitorRef    :: reference(),
    WaitingCalls         :: epool_waiting_calls(),
    WaitingCalls2        :: epool_waiting_calls().

delete_by_caller_monitor_ref(CallerMonitorRef, [#epool_waiting_call{caller_monitor_ref = CallerMonitorRef} | T]) -> T;
delete_by_caller_monitor_ref(CallerMonitorRef, [H | T]) -> [H | delete_by_caller_monitor_ref(CallerMonitorRef, T)];
delete_by_caller_monitor_ref(_CallerMonitorRef, []) -> [].


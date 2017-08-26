%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%% Application main module
%%% @end
%%%-------------------------------------------------------------------
-module(epool_app).
-author("Madalin Grigore-Enescu").

-include_lib("epool/include/epool.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

%% @spec start(Type, Args) -> ServerRet
%% @doc application start callback.
start(_Type, _Args) ->

    %% Get main supervisor name
    %% Allows easy customization of the epool main supervisor name without altering the source code
    MainSupName = application:get_env(epool, sup_name, ?EPOOL_DEFAULT_SUP_NAME),

    %% Start application main supervisor
    case epool_sup:start_link(MainSupName, []) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

%% @spec stop(State) -> ServerRet
%% @doc application stop callback.
stop(_State) -> ok.


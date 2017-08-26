%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, Madalin Grigore-Enescu <os@ergenius.com>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2017 6:41 AM
%%%-------------------------------------------------------------------
-module(epool_test).
-author("Madalin Grigore-Enescu").

%% Including the EUnit header file
%%
%% The simplest way to use EUnit in an Erlang module is to add the following line at the beginning of the module
%% (after the -module declaration, but before any function definitions):
%%
%% -include_lib("eunit/include/eunit.hrl").
%% This will have the following effect:
%%
%% Creates an exported function test() (unless testing is turned off, and the module does not already contain a test() function),
%% that can be used to run all the unit tests defined in the module
%%
%% Causes all functions whose names match ..._test() or ..._test_() to be automatically exported from the module
%% (unless testing is turned off, or the EUNIT_NOAUTO macro is defined)
%%
%% Makes all the preprocessor macros of EUnit available, to help writing tests
%%
%% Avoiding compile-time dependency on EUnit
%%
%% If you are distributing the source code for your application for other people to compile and run,
%% you probably want to ensure that the code compiles even if EUnit is not available.
%% You can put the following lines in a common header file:
%%
%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%% -endif.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Include epool.hrl
-include_lib("epool/include/epool.hrl").

%% Writing test generating functions
%%
%% A drawback of simple test functions is that you must write a separate function (with a separate name) for each test case.
%% A more compact way of writing tests (and much more flexible, as we shall see), is to write functions that return tests, instead of being tests.
%%
%% A function with a name ending in ..._test_() (note the final underscore) is recognized by EUnit as a test generator function.
%% Test generators return a representation of a set of tests to be executed by EUnit.
%%
%% Representing a test as data The most basic representation of a test is a single fun-expression that takes no arguments.
%% For example, the following test generator:
%%
%% basic_test_() ->
%% fun () -> ?assert(1 + 1 =:= 2) end.
%% will have the same effect as the following simple test:
%% simple_test() ->
%% ?assert(1 + 1 =:= 2).
%% (in fact, EUnit will handle all simple tests just like it handles fun-expressions: it will put them in a list, and run them one by one).

%% Test epool as a separate application
application_test_() ->

    {setup,
     fun() ->

         %% Set some testing epool application environment
         application:set_env(epool, metrics_module, fake_metrics)

     end,
     fun(_X) ->
        ok
     end,

     %% foreach is used to set up a fixture and optionally tear it down afterwards,
     %% repeated for each single one of the specified test sets.
     %%
     %% A Setup function is executed just before any of the specified tests are run, and a Cleanup function is executed when no more of
     %% the specified tests will be run, regardless of the reason. A Setup function takes no argument, and returns some value which
     %% will be passed as it is to the Cleanup function. A Cleanup function should do whatever necessary and return some arbitrary value,
     %% such as the atom ok. (SetupX and CleanupX functions are similar, but receive one additional argument:
     %% some value X, which depends on the context.)
     {foreach,

      %% Setup function
      fun() ->

          %% Define some testing pools
          Pools = [
              [
                  {name, pool_1},
                  {group, undefined},
                  {min_size, 2},
                  {max_size, {0, min}},
                  {strategy, lifo},
                  {worker_module, epool_test_srv},
                  {worker_args, test}
              ]
          ],

          %% Set the testing pools into epool application environment
          application:set_env(pooler, pools, Pools),

          %% Start epool application for the test
          application:start(epool)

      end,

      %% Cleanup function
      fun(_) ->

          %% Stop epool application when test finish
          application:stop(epool)

      end,

      %% Run all tests
      all_tests()

      }}.

%% Test epool as a library with custom supervision
library_test_() -> ok.

all_tests() ->

    [
        %% Any test or test set T can be annotated with a title, by wrapping it in a pair {Title, T},
        %% where Title is a string. For convenience, any test which is normally represented using a tuple
        %% can simply be given a title string as the first element, i.e., writing {"The Title", ...}
        %% instead of adding an extra tuple wrapper as in {"The Title", {...}}.

        {<<"Test busy workers based on ets lookup">>,
         fun test_epool_x_busy_workers_ets_lookup/0
        }

    ].

test_epool_x_busy_workers_ets_lookup() -> ok.






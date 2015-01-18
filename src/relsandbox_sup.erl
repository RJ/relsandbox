%%%-------------------------------------------------------------------
%% @doc relsandbox top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relsandbox_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Counter = {relsandbox_counter,
               {relsandbox_counter, start_link, []},
               permanent, 5000, worker, [relsandbox_counter]},

    Phonebook = {relsandbox_phonebook,
                 {relsandbox_phonebook, start_link, []},
                 permanent, 5000, worker, [relsandbox_phonebook]},

    Children = [Counter, Phonebook],

    {ok, {{one_for_one, 10, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

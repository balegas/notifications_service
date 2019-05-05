%%%-------------------------------------------------------------------
%% @doc antidote_notifications top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(antidote_notifications_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Manager = #{
    id => notifications_manager_a,
    start => {notifications_manager, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [notifications_manager]},

  WorkerSup = #{
    id => workers_sup,
    start => {workers_sup, start_link, [{worker, start_link, []}]},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [workers_sup]},

  {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [Manager, WorkerSup]}}.

stop() ->
  supervisor:terminate_child(?SERVER, notifications_manager),
  supervisor:delete_child(?SERVER, notifications_manager),
  supervisor:terminate_child(?SERVER, workers_sup),
  supervisor:delete_child(?SERVER, workers_sup).


%%====================================================================
%% Internal functions
%%====================================================================

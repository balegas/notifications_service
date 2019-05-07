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


%%====================================================================
%% API
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Manager = #{
    id => notifications_manager_proc,
    start => {notifications_manager, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [notifications_manager]},

  WorkerSup = #{
    id => workers_mgr_proc,
    start => {workers_mgr, start_link, [{worker, start_link, []}]},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [workers_mgr]},

  {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [Manager, WorkerSup]}}.

stop() ->
  supervisor:terminate_child(?MODULE, notifications_manager_proc),
  supervisor:delete_child(?MODULE, notifications_manager_proc),
  supervisor:terminate_child(?MODULE, workers_sup_proc),
  supervisor:delete_child(?MODULE, workers_sup_proc),
  case whereis(antidote_notifications_sup) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.


%%====================================================================
%% Internal functions
%%====================================================================

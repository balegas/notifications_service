%%%-------------------------------------------------------------------
%% @doc antidote_notifications top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(antidote_notifications_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(NOTIFICATIONS_MGR_MODULE, notifications_manager).
-define(WORKERS_MGR_MODULE, workers_manager).
-define(ROUTER_MODULE, router_fanout).

%%====================================================================
%% API
%%====================================================================

start_link(AMQPChannel) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [AMQPChannel]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([EventSource]) ->
  NotificationsManager = #{
    id => notifications_manager_proc,
    start => {?NOTIFICATIONS_MGR_MODULE, start_link, [?ROUTER_MODULE]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?NOTIFICATIONS_MGR_MODULE]},

  WorkerManager = #{
    id => workers_mgr_proc,
    start => {?WORKERS_MGR_MODULE, start_link, [{worker, start_link, []}]},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [?WORKERS_MGR_MODULE]},

  Router = #{
    id => router_proc,
    start => {?ROUTER_MODULE, start_link, [master, EventSource]},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [?ROUTER_MODULE]},

  {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [NotificationsManager, WorkerManager, Router]}}.

stop() ->
  case whereis(antidote_notifications_sup) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.


%%====================================================================
%% Internal functions
%%====================================================================

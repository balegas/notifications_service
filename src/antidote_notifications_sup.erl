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
    id => notifications_manager,
    start => {notifications_manager, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [notifications_manager]},

  {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [Manager]}}.

stop() ->
  supervisor:terminate_child(?MODULE, notifications_manager),
  supervisor:delete_child(?MODULE, notifications_manager).


%%====================================================================
%% Internal functions
%%====================================================================

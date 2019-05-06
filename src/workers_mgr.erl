%%%-------------------------------------------------------------------
%%% @author vbalegas
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2019 17:40
%%%-------------------------------------------------------------------
-module(workers_mgr).

-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1, terminate/2, spawn_worker/1]).
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link(MFA = {_, _, _}) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, MFA).

terminate(_Reason, _State) ->
  ?LOG_INFO("Worker supervisor terminate"),
  ok.

init({Module, Function, Args}) ->
  {ok, {#{strategy => simple_one_for_one, intensity => 5, period => 10},
    [#{
      id => pusher_worker,
      start => {Module, Function, Args},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [Module]
    }]
  }
  }.

spawn_worker(Connection) ->
  supervisor:start_child(?MODULE, [Connection]).
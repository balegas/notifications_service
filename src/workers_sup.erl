%%%-------------------------------------------------------------------
%%% @author vbalegas
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2019 17:40
%%%-------------------------------------------------------------------
-module(workers_sup).

-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1, terminate/2]).
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link(MFA = {_, _, _}) ->
  supervisor:start_link({local, workers_sup}, ?MODULE, MFA).

terminate(_Reason, _State) ->
  ?LOG_INFO("Worker supervisor terminate"),
  ok.

init({Module, Function, Args}) ->
  {ok, {#{strategy => simple_one_for_one, intensity => 5, period => 10},
    [#{
      id => pusher_worker,
      start => {Module, Function, Args},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]
    }]
  }
  }.



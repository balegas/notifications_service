-module(router).
-include_lib("kernel/include/logger.hrl").
-include_lib("../include/records.hrl").

%% API
-export([add_endpoint/2]).

%%====================================================================
%% API
%%====================================================================
add_endpoint(Endpoint, Router) ->
  gen_server:call(Router, {add_endpoint, Endpoint}).

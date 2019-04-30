%%%-------------------------------------------------------------------
%% @doc antidote_notifications public API
%% @end
%%%-------------------------------------------------------------------

-module(antidote_notifications_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    antidote_notifications_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

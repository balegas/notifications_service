-module(antidote_notifications_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, antidote_notifications, "index.html"}},
            {"/websocket", websocket_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, antidote_notifications, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    %%lager:start(),
    antidote_notifications_sup:start_link().

stop(_State) ->
    antidote_notifications_sup:stop(),
    ok.

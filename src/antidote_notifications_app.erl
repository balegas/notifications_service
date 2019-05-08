-module(antidote_notifications_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include_lib("amqp_client/include/amqp_client.hrl").

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

  {ok, AMQPClient} = amqp_connection:start(#amqp_params_network{
    username = application:get_env(amqp, username, <<"guest">>),
    password = application:get_env(amqp, password, <<"guest">>),
    virtual_host = application:get_env(amqp, password, <<"/">>),
    %%TODO Learn about env variables
    host = application:get_env(amqp, host, "192.168.1.68"),
    port = application:get_env(amqp, port, undefined),
    channel_max = application:get_env(amqp, channel_max, 2047),
    frame_max = application:get_env(amqp, frame_max, 0),
    heartbeat = application:get_env(amqp, heartbeat, 10),
    connection_timeout = application:get_env(amqp, connection_timeout, 60000)
  }),

  {ok, Channel} = amqp_connection:open_channel(AMQPClient),

  Declare = #'exchange.declare'{exchange = <<"test_exchange">>},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = <<"test_queue">>}),

  Binding = #'queue.bind'{queue = Queue,
    exchange = Declare#'exchange.declare'.exchange
  },
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

  antidote_notifications_sup:start_link(Channel).

stop(_State) ->
  antidote_notifications_sup:stop(),
  ok.

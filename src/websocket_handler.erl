-module(websocket_handler).
-include_lib("kernel/include/logger.hrl").
-include_lib("../include/records.hrl").

-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-define(JSON_PARSER, json_message_converter).
-define(DECODE_JSON(X), ?JSON_PARSER:to_tuple(X)).
-define(ENCODE_JSON(X), ?JSON_PARSER:to_binary(X)).

-record(state, {worker}).

%%====================================================================
%% API
%%====================================================================

init(Req, State) ->
  ?LOG_INFO("Websocket upgrade init Req: ~p Opts: ~p", [Req, State]),
  {cowboy_websocket, Req, State, #{idle_timeout => 3600000}}.

terminate(_, _, _) ->
  ?LOG_INFO("Websocket connection terminated"),
  ok.

%%====================================================================
%% Callbacks
%%====================================================================

websocket_init(State) ->
  ?LOG_INFO("Websocket connection init. Current State ~p", [State]),
  {ok, Pid} = gen_server:call(notifications_manager, {new_connection, self(), []}),
  ?LOG_INFO("Websocket worker pid ~p", [Pid]),
  {ok, #state{worker = Pid}}.


websocket_handle({text, Msg}, State) ->
  {reply, {text, <<"Echo Msg! ", Msg/binary>>}, State};

websocket_handle(Msg, State) ->
  ?LOG_INFO("Unexpected Message ~p ~p", [Msg, State]),
  {ok, State}.


websocket_info({deliver, #updateEvent{payload = Payload}}, State) ->
  {reply, {text, ?ENCODE_JSON(Payload)}, State};


websocket_info(Msg, State) ->
  ?LOG_INFO("Unexpected Message ~p ~p", [Msg, State]),
  {ok, State}.

%%TODO: Use Request Accept headers to select appropriate serialization engine
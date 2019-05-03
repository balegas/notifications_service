-module(websocket_handler).

-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(JSON_PARSER, json_message_converter).
-define(DECODE_JSON(X), ?JSON_PARSER:to_tuple(X)).
-define(ENCODE_JSON(X), ?JSON_PARSER:to_binary(X)).

-record(state, {worker}).

%%====================================================================
%% API
%%====================================================================

init(Req, Opts) ->
  ?LOG_INFO("Websocket upgrade init Req: ~p Opts: ~p", [Req, Opts]),
  {cowboy_websocket, Req, Opts}.

terminate(_, _, _) ->
  ?LOG_INFO("Websocket connection terminated"),
  ok.

%%====================================================================
%% Callbacks
%%====================================================================

websocket_init(State) ->
  ?LOG_INFO("Websocket connection init. Current State ~p",[State]),
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, registered, Pid} = gen_server:call(notifications_manager, new_connection),
  ?LOG_INFO("Websocket worker pid ~p", [Pid]),
  {ok, #state{worker = Pid}}.

websocket_handle({text, Msg}, State) ->
  ?LOG_INFO("Handle ~p.", [?DECODE_JSON(Msg)]),
  {reply, {text, <<"That's what she said! ", Msg/binary>>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

%%TODO: Use Request Accept headers to select appropriate serialization engine
-module(websocket_handler).

-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(JSON_PARSER, json_message_converter).
-define(DECODE_JSON(X), ?JSON_PARSER:to_tuple(X)).
-define(ENCODE_JSON(X), ?JSON_PARSER:to_binary(X)).

%%====================================================================
%% Callbacks
%%====================================================================

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  ?LOG_INFO("Websocket connection init"),
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, Res} = gen_server:call(notifications_manager, {new_connection}),
  ?LOG_INFO("Websocket connection result ~p", [Res]),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  ?LOG_INFO("Handle ~p.", [?DECODE_JSON(Msg)]),
  {reply, {text, <<"That's what she said! ", Msg/binary>>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

%websocket_info({registered},  State) ->
%  {ok, State};
websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate(_, _, _) ->
  ?LOG_INFO("Websocket connection terminated"),
  ok.

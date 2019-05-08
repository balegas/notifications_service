-module(router_fanout).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("../include/records.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/2, terminate/1, add_endpoint/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


%%====================================================================
%% API
%%====================================================================

start_link(Type, EventSource) ->
  ?LOG_INFO("Fanout Router start"),
  gen_server:start_link(?MODULE, {Type, EventSource}, []).

terminate(Router) ->
  gen_server:call(Router, terminate).

add_endpoint(Endpoint, Router) ->
  gen_server:call(Router, {add_endpoint, Endpoint}).

%%====================================================================
%% Callbacks
%%====================================================================

init({master, EventSource}) ->
  register(?MODULE, self()),
  init({EventSource});

init({EventSource}) ->
  ?LOG_INFO("EventSource ~p. Self Pid ~p", [EventSource, self()]),
  #'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:call(EventSource, #'basic.consume'{queue = <<"test_queue">>}),
  %%TODO: Store tag
  {ok, #fanoutState{event_source = EventSource, endpoints = sets:new()}}.

handle_call({add_endpoint, Endpoint}, _From, #fanoutState{endpoints = Eps} = State) ->
  ?LOG_INFO("Adding new endpoint: ~p ~p. self pid ~p", [Endpoint, State, self()]),
  {reply, ok, State#fanoutState{endpoints = sets:add_element(Endpoint, Eps)}};

handle_call(Msg, _From, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {reply, unknown_msg, State}.

handle_cast(#updateEvent{} = Event, #fanoutState{endpoints = Endpoints} = State) ->
  [gen_server:cast(E, Event) || E <- Endpoints],
  {norepy, State};

handle_cast(Msg, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
  ?LOG_INFO("Basic consume OK"),
  {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
  ?LOG_INFO("Basic consume cancel. Must do something with connection"),
  {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #fanoutState{event_source = ES, endpoints = Eps} = State) ->
  ?LOG_INFO("Basic Delivery ~p", [Payload]),
  %%TODO: What is the type of metadata?
  [worker:deliver_notification(Payload, #{}, E) || E <- sets:to_list(Eps)],
  amqp_channel:cast(ES, #'basic.ack'{delivery_tag = Tag}),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_INFO("Unexpected message or timeout (check timeout): ~p ~p", [Msg, State]),
  {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-module(notifications_manager).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, add_client/2, delete_client/2, push_notification/1]).
%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(notification, {source, destination, payload, meta}).
-record(state, {subscriptions}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

add_client(Pid, Subscriptions) -> gen_server:call(?MODULE, {new_connection, Pid, Subscriptions}).

delete_client(Pid, Reason) -> gen_server:call(?MODULE, {terminate_connection, Pid, Reason}).

push_notification(_) -> ok.

%%====================================================================
%% Callbacks
%%====================================================================

init([]) ->
  ?LOG_INFO("Notifications manager init"),
  register(notifications_manager, self()),
  {ok, #state{subscriptions = #{}}}.

handle_call({push, _Notification = #notification{}}, _From, State) ->
  {reply, <<"Binary data">>, State};

handle_call({new_connection, Pid, Subscriptions}, _From, State) ->
  {ok, Pid, NewState} = add_client(Pid, Subscriptions, State),
  {reply, {ok, Pid}, NewState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.


handle_cast(_Any, State) ->
  {noreply, State}.


handle_info(Msg, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ?LOG_INFO("Notifications Manager terminate"),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

add_client(Pid, Subscriptions, State = #state{subscriptions = Sub}) ->
  Worker = supervisor:start_child(worker_sup, [Pid]),
  {ok, Worker, State#state{subscriptions = Sub#{Worker => Subscriptions}}}.

%delete_client_internal({_Pid, _Ref}, _State) -> ok.
%%  unlink(Pid),
%%  {ok, State#state{subscriptions = maps:remove(Pid, Sub)}}.

-module(notifications_manager).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, register_client/1, push_notification/1]).
%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(notification, {source, destination, payload, meta}).
-record(state, {subscriptions}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  gen_server:start_link(notifications_manager, [], []).

register_client(_) -> ok.

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

handle_call(new_connection, From, State) ->
  {ok, Pid, NewState} = register_client(From, State),
  {reply, {ok, registered, Pid}, NewState};

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

register_client({Pid, _Ref}, State = #state{subscriptions = Sub}) ->
  {ok, Worker} = supervisor:start_child(workers_sup, [Pid]),
  {ok, Worker, State#state{subscriptions = Sub#{Pid => []}}}.

%%unregister_client({Pid, _Ref}, State = #state{subscriptions = Sub}) ->
%%  unlink(Pid),
%%  {ok, State#state{subscriptions = maps:remove(Pid, Sub)}}.

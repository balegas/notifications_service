-module(notifications_manager).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, stop/0, add_client/2, delete_client/1, push_notification/1]).
%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(notification, {source, destination, payload, meta}).
-record(state, {sub}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, terminate).

%% Subscriptions is a list of strings
add_client(Pid, Keys = [_ | _]) ->
  KeysF = lists:filter(
    fun(E) -> io_lib:char_list(E) end, Keys
  ),
  gen_server:call(?MODULE, {new_connection, Pid, KeysF});

add_client(_, _) -> {error, "Invalid input"}.


delete_client(Pid) -> gen_server:call(?MODULE, {terminate_connection, Pid}).

push_notification(_) -> ok.

%%====================================================================
%% Callbacks
%%====================================================================

init([]) ->
  ?LOG_INFO("Notifications manager init"),
  register(notifications_manager, self()),
  {ok, #state{sub = #{}}}.

handle_call({push, _Notification = #notification{}}, _From, State) ->
  {reply, <<"Binary data">>, State};

handle_call({new_connection, Pid, Subscriptions}, _From, State) ->
  {ok, Worker, NewState} = add_client(Pid, Subscriptions, State),
  {reply, {ok, Worker}, NewState};

handle_call({terminate_connection, Pid}, _From, State) ->
  {ok, NewState} = delete_client(Pid, State),
  {reply, ok, NewState};

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

add_client(Connection, Keys, State = #state{sub = Sub}) ->
  {ok, Worker} = workers_mgr:spawn_worker(Connection),
  KeysSet = lists:foldl(
    fun(E, Acc) -> sets:add_element(E, Acc) end, sets:new(), Keys),
  {ok, Worker, State#state{sub = Sub#{Worker => KeysSet}}}.

delete_client(Worker, #state{sub = Sub} = State) ->
  ok = worker:terminate(Worker),
  {ok, State#state{sub = maps:remove(Worker, Sub)}}.

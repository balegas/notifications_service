-module(notifications_manager).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("../include/records.hrl").

%% API
-export([start_link/1, stop/0, add_client/2, delete_client/1, push_notification/1]).
%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link(Router) ->
  gen_server:start_link(?MODULE, [Router], []).

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

init([Router]) ->
  ?LOG_INFO("Notifications manager init"),
  register(notifications_manager, self()),
  {ok, #mgrState{sub = #{}, con = #{}, router = Router}}.

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

add_client(Connection, Keys, State = #mgrState{sub = Sub, router = Router}) ->
  {ok, Worker} = workers_manager:spawn_worker(Connection),
  router:add_endpoint(Worker, Router),
  KeysSet = lists:foldl(
    fun(E, Acc) -> sets:add_element(E, Acc) end, sets:new(), Keys),

  {ok, Worker, State#mgrState{sub = Sub#{Worker => KeysSet}}}.

delete_client(Worker, #mgrState{sub = Sub} = State) ->
  ok = worker:terminate(Worker),
  {ok, State#mgrState{sub = maps:remove(Worker, Sub)}}.

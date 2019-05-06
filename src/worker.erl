-module(worker).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("../include/records.hrl").

%% API
-export([start_link/1, terminate/1, deliver_notification/2, subscribe/2, subscribe/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Connection) ->
  ?LOG_INFO("Worker start"),
  gen_server:start_link(worker, Connection, []).

terminate(Worker) ->
  gen_server:call(Worker, terminate).

deliver_notification(Ntf, Worker) ->
  gen_server:cast(Worker, Ntf).

subscribe(Subscription, Worker) ->
  subscribe(Subscription, fail, Worker).

subscribe(Subscription, Replace, Worker) ->
  gen_server:call(Worker, {Subscription, Replace}).

init(Connection) ->
  link(Connection),
  process_flag(trap_exit, true),
  {ok, #workerState{con = Connection, sub = #{}}}.

handle_call({#subscription{key = Key, props = Props}, Replace}, _From, #workerState{sub = Sub} = State) ->
  {Res, NewState} =
    case maps:find(Key, Sub) of
      {ok, _Sub} when Replace == fail ->
        {{error, subscription_exists}, State};
      {ok, Val} ->
        PropsUpdt =
          case Replace of
            merge -> maps:merge(Props, Val);
            replace -> Props
          end,
        {ok, #workerState{sub = maps:put(Key, PropsUpdt, Sub)}};
      error -> {ok, #workerState{sub = maps:put(Key, Props, Sub)}}
    end,
  {reply, Res, NewState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_, _From, State) ->
  {reply, unknown_msg, State}.

handle_cast(Msg, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, _State) ->
  gen_server:stop(self());

handle_info(Msg, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {noreply, State}.
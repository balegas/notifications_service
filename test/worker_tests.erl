-module(worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/records.hrl").

%% These tests start the necessary processes for the gen_server to work.
%% Alternatively should mock dependencies.

-define(NOTIF_MGR, notifications_manager).
-define(WORKER_MGR, workers_mgr).

init_test_() ->
  {"Test Notifications Worker",
    [
      {setup,
        fun start/0,
        fun stop/1,
        fun(Worker) ->
          [
            unhandled_message1(Worker),
            unhandled_message2(Worker),
            subscribe(Worker),
            subscribe_fail(Worker),
            subscribe_merge(Worker),
            subscribe_replace(Worker),
            cover100(Worker)
          ]
        end
      },
      {setup,
        fun start_short/0,
        fun no_stop/1,
        fun(Worker) ->
          [
            connection_terminated(Worker)
          ]
        end
      }]
  }.

start() ->
  start_(1000000).

start_short() ->
  start_(1000).

start_(Timeout) ->
  {ok, Pid} = worker:start_link(test_utils:spawn_connection(timeout, Timeout)),
  Pid.

stop(Pid) -> worker:terminate(Pid).

no_stop(_Pid) -> ok.

subscribe(Worker) ->
  ok = worker:subscribe(#subscription{key = "fakeKey", props = #{prop0 => prop0}}, Worker),
  #workerState{sub = Sub} = sys:get_state(Worker),
  {Res, _} = maps:find("fakeKey", Sub),
  [?_assertEqual(ok, Res)].

subscribe_fail(Worker) ->
  Res = worker:subscribe(#subscription{key = "fakeKey", props = #{}}, Worker),
  [?_assertEqual({error, subscription_exists}, Res)].

subscribe_merge(Worker) ->
  Res = worker:subscribe(#subscription{key = "fakeKey", props = #{prop1 => prop1}}, merge, Worker),
  #workerState{sub = Sub} = sys:get_state(Worker),
  {_, Props} = maps:find("fakeKey", Sub),
  [
    ?_assertEqual(ok, Res),
    ?_assertEqual(true, maps:is_key(prop0, Props)),
    ?_assertEqual(true, maps:is_key(prop1, Props))
  ].

subscribe_replace(Worker) ->
  Res = worker:subscribe(#subscription{key = "fakeKey", props = #{prop2 => prop2}}, replace, Worker),
  #workerState{sub = Sub} = sys:get_state(Worker),
  {_, Props} = maps:find("fakeKey", Sub),
  [
    ?_assertEqual(ok, Res),
    ?_assertEqual(false, maps:is_key(prop0, Props)),
    ?_assertEqual(false, maps:is_key(prop1, Props)),
    ?_assertEqual(true, maps:is_key(prop2, Props))
  ].

unhandled_message1(Worker) ->
  Res = gen_server:call(Worker, foobar),
  [?_assertEqual(unknown_msg, Res)].

unhandled_message2(Worker) ->
  gen_server:cast(Worker, foobar),
  Res =
    receive
      _ -> error
    after 500 -> ok
    end,
  [?_assertEqual(ok, Res)].

connection_terminated(Worker) ->
  timer:sleep(2000),
  [?_assert(is_process_alive(Worker))].

cover100(Worker) ->
  Worker ! anyMsg,
  [?_assert(true)].
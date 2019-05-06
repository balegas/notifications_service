-module(notifications_manager_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/records.hrl").

%% These tests start the necessary processes for the gen_server to work.
%% Alternatively should mock dependencies.

-define(NOTIF_MGR, notifications_manager).
-define(WORKER_MGR, workers_mgr).

init_test_() ->
  {"Tests Notifications Manager",
    {setup,
      fun start/0,
      fun stop/1,
      fun(Input) ->
        [
          is_init(Input),
          register_connection_success(Input),
          delete_connection_success(Input)
        ]
      end
    }
  }.

start() ->
  {ok, Pid} = antidote_notifications_sup:start_link(),
  Procs = supervisor:which_children(Pid),
  lists:foldl(fun({Id, ChildPid, _, _}, Acc) -> Acc#{Id => ChildPid} end, #{}, Procs).

stop(_Pids) -> ok.

is_init(Pids) ->
  [
    ?_assert(is_process_alive(maps:get(notifications_manager_proc, Pids))),
    ?_assert(is_process_alive(maps:get(workers_sup_proc, Pids))),
    ?_assertEqual(maps:get(notifications_manager_proc, Pids), whereis(?NOTIF_MGR)),
    ?_assertEqual(maps:get(workers_sup_proc, Pids), whereis(?WORKER_MGR))
  ].

register_connection_success(_Pid) ->
  {ok, Worker} = notifications_manager:add_client(spawn(fun() -> timer:sleep(1000000) end), ["fakeKey"]),
  #mgrState{sub = Sub} = sys:get_state(notifications_manager),
  MySubs = maps:get(Worker, Sub),
  [?_assert(sets:is_element("fakeKey", MySubs))].

delete_connection_success(_Pid) ->
  {ok, Worker} = notifications_manager:add_client(spawn(fun() -> timer:sleep(1000000) end), ["fakeKey"]),
  ok = notifications_manager:delete_client(Worker),
  #mgrState{sub = Sub} = sys:get_state(notifications_manager),
  [?_assertEqual(error, maps:find(Worker, Sub))].

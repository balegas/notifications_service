-module(notifications_manager_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/records.hrl").

-define(NOTIF_MGR, notifications_manager).
-define(WORKER_MGR, workers_manager).


%% These tests start the necessary processes for the gen_server to work.
%% Alternatively should mock dependencies.

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
  test_utils:start_application(),
  #{?NOTIF_MGR => whereis(?NOTIF_MGR), ?WORKER_MGR => whereis(?WORKER_MGR)}.

stop(_Pids) -> ok.

is_init(Pids) ->
  [
    ?_assert(is_process_alive(maps:get(?NOTIF_MGR, Pids))),
    ?_assert(is_process_alive(maps:get(?WORKER_MGR, Pids))),
    ?_assertEqual(maps:get(?NOTIF_MGR, Pids), whereis(?NOTIF_MGR)),
    ?_assertEqual(maps:get(?WORKER_MGR, Pids), whereis(?WORKER_MGR))
  ].

register_connection_success(_Pid) ->
  {ok, Worker} = ?NOTIF_MGR:add_client(spawn(fun() -> timer:sleep(1000000) end), ["fakeKey"]),
  #mgrState{sub = Sub} = sys:get_state(?NOTIF_MGR),
  MySubs = maps:get(Worker, Sub),
  [?_assert(sets:is_element("fakeKey", MySubs))].

delete_connection_success(_Pid) ->
  {ok, Worker} = ?NOTIF_MGR:add_client(spawn(fun() -> timer:sleep(1000000) end), ["fakeKey"]),
  ok = ?NOTIF_MGR:delete_client(Worker),
  #mgrState{sub = Sub} = sys:get_state(?NOTIF_MGR),
  [?_assertEqual(error, maps:find(Worker, Sub))].

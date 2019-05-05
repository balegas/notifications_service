-module(notifications_manager_tests).
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% All Test Fixtures
init_test() ->
  {"Tests Notifications Manager",
    {setup,
      fun start/0,
      fun stop/1,
      fun is_init/1
    }
  }.

start() -> {ok, Pid}  = notifications_manager:start_link(), Pid.

stop(Pid) -> notifications_manager:stop(Pid).

is_init(Pid) ->
  [?assert(is_process_alive(Pid)),
  ?assertEqual(Pid, whereis(notifications_manager))].
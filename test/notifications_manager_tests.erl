-module(notifications_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%% All Test Fixtures
init_test_() ->
  {"Tests Notifications Manager",
    {setup,
      fun start/0,
      fun stop/1,
      fun is_init/1
    }
  }.

start() ->
  {ok, Pid} = notifications_manager:start_link(),
  Pid.

stop(_Pid) -> notifications_manager:stop().

is_init(Pid) ->
  [?_assert(is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(notifications_manager))].
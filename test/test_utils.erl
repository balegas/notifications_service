%%%-------------------------------------------------------------------
%%% @author vbalegas
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 13:14
%%%-------------------------------------------------------------------
-module(test_utils).
-author("vbalegas").

%% API
-export([start_application/0, spawn_connection/2]).


start_application() ->
  application:start(crypto),
  application:start(cowlib),
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  application:start(ranch),
  application:start(cowboy),
  application:start(compiler),
  application:start(syntax_tools),
  application:start(antidote_notifications).

spawn_connection(timeout, Timeout) ->
  spawn(fun() -> timer:sleep(Timeout), exit(normal) end).
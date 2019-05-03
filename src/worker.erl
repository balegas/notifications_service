%%%-------------------------------------------------------------------
%%% @author vbalegas
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2019 19:30
%%%-------------------------------------------------------------------
-module(worker).
-author("vbalegas").

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1, terminate/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {connection_handler}).

start_link(Pid) ->
  ?LOG_INFO("Worker start"),
  gen_server:start_link(worker, [Pid], []).

terminate(_,_) ->
  ?LOG_INFO("Worker terminate"),
  ok.

init([Pid]) ->
  link(Pid),
  process_flag(trap_exit, true),
  {ok, #state{connection_handler = Pid}}.

handle_call(_, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_INFO("Unexpected message: ~p ~p", [Msg, State]),
  {noreply, State}.


%%TODO: Terminate when child normal exit is trapped.
%%TODO: Worker does all the logic independent of underlying channel, connection_handler does propagation only.
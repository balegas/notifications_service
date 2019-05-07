%%%-------------------------------------------------------------------
%%% @author vbalegas
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2019 00:15
%%%-------------------------------------------------------------------
-module(json_message_converter).
-author("vbalegas").

%% API
-export([to_tuple/1, to_binary/1]).


%%====================================================================
%% API
%%====================================================================

to_tuple(Msg) ->
  jiffy:decode(Msg).

to_binary(Tuple) ->
  jiffy:encode(Tuple).

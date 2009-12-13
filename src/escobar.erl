%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 12 Dec 2009 by mats cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('escobar').
-author('mats cronqvist').
-export([log/2]).

log(PI,T) when is_list(T) ->
  error_logger:info_report([proplist:get_val(current_function,PI)||T]);
log(PI,T) ->
  log(PI,[T]).

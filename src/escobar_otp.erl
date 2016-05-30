%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 23 May 2016 by mats cronqvist <mats.cronqvist@gmail.com>

%% @doc
%% @end

-module('escobar_otp').
-author('mats cronqvist').
-export([go/0]).

base(Tail) ->
  string:join(["http://erlang.org/doc",Tail],"/").

go() ->
  Apps = get_apps(),
  get_mf(Apps).

get_mf(Apps) ->
  lists:append(
    [lists:reverse(trane:wget_sax(base(A),fun sxm/2,[])) || A <- Apps]).

sxm({tag,"li",Attrs},A) ->
  case proplists:get_value("title",Attrs) of
    undefined -> A;
    M ->
      case proplists:get_value("expanded",Attrs) of
        "false" ->
          [{m,M}|A];
        undefined ->
          case re:run(M,"[a-z][a-zA-Z0-9_]+-[0-9]+") of
            {match,[{0,_}]} -> [{f,M}|A];
            _ -> A
          end
      end
  end;
sxm(_,A) -> A.

get_apps() ->
  lists:usort(trane:wget_sax(base("applications.html"),fun sx/2, [])).

sx({tag,"a",Attrs},A) -> [proplists:get_value("href",Attrs)|A];
sx(_,A) -> A.


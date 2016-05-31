%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 23 May 2016 by mats cronqvist <mats.cronqvist@gmail.com>

%% @doc
%% crawl http://erlang.org/doc/applications.html to find all exported
%% functions in OTP.
%% Return [{Mod,[Fun-Ari...]}...]
%% @end

-module('escobar_otp').
-author('mats cronqvist').
-export([go/0]).

base(Tail) ->
  string:join(["http://erlang.org/doc",Tail],"/").

go() ->
  Apps = get_apps(),
  MFs = get_mf(Apps),
  mk_mfa(MFs,[]).

mk_mfa([],O) ->
  O;
mk_mfa([{m,_}],O) ->
  O;
mk_mfa([{m,M},{f,F}|MFs],O) ->
  mk_mfa(MFs,[{M,[F]}|O]);
mk_mfa([{m,_},{m,M}|MFs],O) ->
  mk_mfa([{m,M}|MFs],O);
mk_mfa([{f,F}|MFs],[{M,Fs}|O]) ->
  mk_mfa(MFs,[{M,[F|Fs]}|O]).

get_mf(Apps) ->
  lists:append(
    [lists:reverse(trane:wget_sax(base(A),fun sxm/2,[])) || A <- Apps]).

sxm({tag,"li",Attrs},A) ->
  case proplists:get_value("title",Attrs) of
    undefined -> A;
    I ->
      case proplists:get_value("expanded",Attrs) of
        "false" ->
          [M] = string:tokens(I," "),
          [{m,M}|A];
        undefined ->
          case re:run(I,"[a-z][a-zA-Z0-9_]+-[0-9]+") of
            {match,[{0,_}]} -> [{f,I}|A];
            _ -> A
          end
      end
  end;
sxm(_,A) -> A.

get_apps() ->
  lists:usort(trane:wget_sax(base("applications.html"),fun sx/2, [])).

sx({tag,"a",Attrs},A) -> [proplists:get_value("href",Attrs)|A];
sx(_,A) -> A.

% -*- mode: erlang; erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% File    : escobar.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created :  5 Oct 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(escobar).

-include_lib("kernel/include/file.hrl").

-export([go/0,log/2,do_find_inc/2]).

-import(lists,[foldl/3,append/1,foreach/2,reverse/1,member/2]).
-import(dict,[new/0,fetch/2,append/3]).

-define(LOG(T),escobar:log(process_info(self()),T)).

go() ->
    Filename = join([os:getenv("HOME"),".escobar","conf.txt"]),
    case file:consult(Filename) of
        {ok,Terms} ->
            Conf = foldl(fun conf/2,new(),Terms),
            cp_css_file(Conf),
            mk_xrefs(Conf),
            unresolved_includes(Conf),
            proc_xrefs(Conf),
            mk_otp(Conf),
            mk_htmls(Conf);
        {error,enoent} ->
            {no_config_file,Filename}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_htmls(Conf) ->
    %% here we need to figure out which files each html file depends on
    %% for now we assume the html file depends only on the {eh}rl file
    Dests = fetch(destination,Conf),
    [Dest] = Dests,
    Files = up2date(Dest,Dests,"\\.xrz\$",{".xrz",".html"}),
    foreach(fun html/1, Files).

html({XrzFile,HtmlFile}) ->
        {ok,FD} = file:open(HtmlFile,[write]),
        put(fd,FD),
        Html = get_html(XrzFile),
        write({html,[],
               [{head,[],
                 [{link,[{rel,"stylesheet"},
                         {type,"text/css"},
                         {href,"escobar.css"}],[]}]},
                {body,[],
                 [{'div',[{class,"code"}],
                   [{pre,[],[{raw,Html}]}]}]}]}),
        file:close(FD),
        io:fwrite("wrote ~s~n",[HtmlFile]).

get_html(XrzFile) ->
    {ok,FD} = file:open(XrzFile,[read,compressed]),
    try
        {ok,{filename,RL}} = io:read(FD,''),
        escobar_tree:html(escobar_file:get_tree(RL),RL)
    after
        file:close(FD)
    end.

write({raw,Body}) -> out("~s~n",Body);
write({Tag,Attrs,Body}) -> begt(Tag,Attrs),foreach(fun write/1,Body),endt(Tag).

begt(Tag,Attrs) -> out("<~s ",a2l(Tag)),foreach(fun attr/1, Attrs),out(">").

attr({Nam,Val}) -> out("~s=",a2l(Nam)), out("~p ",Val).

endt(Tag) -> out("</~s>~n",a2l(Tag)).

out(Form) -> dout(Form,[]).
out(Form,Val) -> dout(Form,[Val]).
dout(Form,Vals) -> io:fwrite(get(fd),Form,Vals).

a2l(A) -> atom_to_list(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc_xrefs(Conf) ->
    [Dest] = fetch(destination,Conf),
    escobar_xref:build(Dest),
    dump_xrefs(Dest).

dump_xrefs(Dest) ->
    Filename = filename:join(Dest,"xrefs.html"),
    MfaFileLines = escobar_xref:find(global_call),
    {ok,FD} = file:open(Filename,[write]),
    io:fwrite(FD,"<pre>~n",[]),
    foldl(fun(M,A) -> dump_writer(FD,M,A) end,"",MfaFileLines),
    io:fwrite("wrote ~s.~n",[Filename]).

dump_writer(FD,{{M,F,A},File,Lines},Prev) ->
    case {M,F,A} == Prev of
      true  -> dump_lines(FD,File,Lines);
      false ->
        io:fwrite(FD,"<a name=\"~w:~w/~w\">~w:~w/~w</a>~n",[M,F,A,M,F,A]),
        dump_lines(FD,File,Lines)
    end,
    {M,F,A}.

dump_lines(FD,File,Lines) ->
    foreach(fun(L) -> dump_lines_f(FD,File,L) end,Lines).

dump_lines_f(FD,File,L) ->
    io:fwrite(FD,"  <a href=\"~s.html#~w\">~s:~w</a>~n",[File,L,File,L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unresolved_includes(Conf) ->
    [Dest] = fetch(destination,Conf),
    Xrzs = filelib:wildcard(filename:join(Dest,"*xrz")),
    Cs = lists:foldl(fun(F,A) -> {ok,C} = file:consult(F), [C|A] end,[],Xrzs),
    Incs = lists:append([find_inc(FN,Xs) || [{filename,FN},Xs] <- Cs]),
    case lists:usort([maybe_update(Dest,I) || I <- Incs]) of
        [old] -> ok;
        [] -> ok;
        _ -> unresolved_includes(Conf)
    end.

maybe_update(Dest,Longname) ->
    XrzName = xrz_name(Dest,Longname),
    case file:read_file_info(XrzName) of
        {ok,_} -> old;
        _ -> update_xref({Longname,XrzName})
    end.

find_inc(Src,Xs) ->
    [do_find_inc(Src,{T,I}) || {{T,I},_} <- Xs,
                               lists:member(T,[include,include_lib])].

do_find_inc(Src,{include,File}) ->
    AbsName = normalize(filename:join(filename:dirname(Src),File)),
    case file:read_file_info(AbsName) of
        {ok,_} -> AbsName;
        _ -> exit({no_include_file,AbsName})
    end;
do_find_inc(_,{include_lib,File}) ->
    case string:tokens(File,"/") of
        [App,Include,Basename] ->
            case code:lib_dir(App) of
              {error,bad_name} -> exit({no_include_lib,App});
              AppDir ->
                  AbsName = filename:join([AppDir,Include,Basename]),
                  case file:read_file_info(AbsName) of
                      {ok,_} -> AbsName;
                      _ -> exit({no_include_file,AbsName})
                  end
            end
    end.

normalize(Filename) ->
    "/"++string:join(normal(string:tokens(Filename,"/")),"/").

normal([])         -> [];
normal([_,".."|T]) -> normal(T);
normal(["."|T])    -> normal(T);
normal([E|T])      -> [E]++normal(T).

xrz_name(Dest,Filename) ->
    filename:join(Dest,string:join(string:tokens(Filename,"/"),".")++".xrz").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_xrefs(Conf) ->
    %% we find all erl files in 'targets', and generate/update an
    %% erl.xrz file
    [Dest] = fetch(destination,Conf),
    Targs = fetch(targets,Conf),
    Files = up2date(Dest,Targs,"\\.erl\$",{"",".xrz"}),
    foreach(fun update_xref/1, Files).

update_xref({Targ,Dest}) ->
    io:fwrite("~s...",[Dest]),
    Xrefs = reverse(escobar_mk_xref:go(Targ)),
    {ok,FD} = file:open(Dest,[write]),
    io:fwrite(FD,"{filename,~p}.~n",[Targ]),
    io:fwrite(FD,"~p.~n",[Xrefs]),
    file:close(FD),
    io:fwrite(" ~p~n",[length(Xrefs)]).

up2date(Dest,Targs,RegExp,Exts) ->
    append([up2dat(T,RegExp,Dest,Exts) || T <- Targs]).

up2dat(From,RegExp,To,Exts) ->
    FF_F = fun(I,A) -> u2d(I,A,To,Exts) end,
    filelib:fold_files(From,RegExp,true,FF_F,[]).

u2d(Targ,Acc,To,{OldExt,NewExt}) ->
    Dest = join([To,filename:basename(Targ,OldExt)])++NewExt,
    try up2d(Targ,Dest), Acc
    catch _:_ -> [{Targ,Dest}|Acc]
    end.

up2d(Targ,Dest) ->
    {ok,#file_info{mtime=MT}} = file:read_file_info(Targ),
    case file:read_file_info(Dest) of
        {ok,#file_info{mtime=DestMT}} when MT < DestMT -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_otp(Conf) ->
    O = get_otp_data(Conf),
    [[ets:insert(escobar,{{otp,M,F}}) || F <- Fs] || {M,Fs} <- O].

get_otp_data(Conf) ->
    [Dest] = fetch(destination,Conf),
    Dst = filename:join(Dest,"otp.term"),
    case file:read_file_info(Dst) of
        {ok,_} ->
            {ok,[O]} = file:consult(Dst),
            io:fwrite("read ~s~n",[Dst]),
            O;
        _ ->
            O = escobar_otp:go(),
            {ok,FD} = file:open(Dst,[write]),
            io:fwrite(FD,"~p.",[O]),
            file:close(FD),
            io:fwrite("wrote ~s~n",[Dst]),
            O
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cp_css_file(Conf) ->
    [Dest] = fetch(destination,Conf),
    Src = filename:join(code:priv_dir(escobar),"escobar.css"),
    Dst = filename:join(Dest,"escobar.css"),
    {ok,_} = file:copy(Src,Dst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conf({target,From},Conf) -> append(targets,chk_dir(From),Conf);
conf({destination,To},Conf) -> append(destination,ass_dir(To),Conf);
conf(X,_) -> throw({bad_conf_item,X}).

chk_dir(Dir) ->
    try
        true = filelib:is_dir(Dir),
        Dir
    catch _:R -> throw({bad_target,Dir,R})
    end.

ass_dir(Dir) ->
    try
        ok = filelib:ensure_dir(Dir++"/dummy"),
        Dir
    catch
        _:R -> throw({bad_target,Dir,R})
    end.

join(Toks) -> join(Toks,"/").
join([Pref|Toks], Sep) -> foldl(fun(Tok,O) -> O++Sep++Tok end, Pref, Toks).

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    case member(stack,List) of
        true -> log(ProcInfo,List++stk()--[stack]);
        false -> error_logger:info_report(cf(ProcInfo)++List)
    end.

stk() -> stk(erlang:get_stacktrace()).
stk([]) -> [];
stk(Stk) -> [{stack,Stk}].
cf(ProcInfo) -> [{in,CF} || {current_function,CF} <- ProcInfo].

%%%-------------------------------------------------------------------
%%% File    : escobar.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created :  5 Oct 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(escobar).

-include_lib("kernel/include/file.hrl").

-export([go/0,log/2]).

-import(lists,[foldl/3,append/1,foreach/2,reverse/1,member/2]).
-import(dict,[new/0,fetch/2,append/3]).

-define(LOG(T),escobar:log(process_info(self()),T)).

go() ->
    {ok,Terms} = file:consult(join([os:getenv("HOME"),".escobar","conf.txt"])),
    Conf = foldl(fun conf/2,new(),Terms),
    mk_xrefs(Conf),
    proc_xrefs(Conf),
    mk_htmls(Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_htmls(Conf) ->
    %% here we need to figure out which files each html file depends on
    %% for now we assume the html file depends only on the {eh}rl file
    FromTos = [{Dest,Dest} || {_,Dest} <- fetch(targets,Conf)],
    Files = up2date(FromTos,"\\.xrz\$",{".xrz",".html"}),
    foreach(fun html/1, Files).

html({XrzFile,HtmlFile}) ->
    try
        Html = get_html(XrzFile),
        {ok,FD} = file:open(HtmlFile,[write]),
        put(fd,FD),
        try
            write({html,[],
                   [{head,[],
                     [{link,[{rel,"stylesheet"},
                             {type,"text/css"},
                             {href,"escobar.css"}],[]}]},
                    {body,[],
                     [{'div',[{class,"code"}],
                       [{pre,[],[{raw,Html}]}]}]}]})
        after file:close(FD)
        end
    catch
        throw:no_file -> file:delete(XrzFile);
          _:R -> ?LOG([htmlification_failed,{file,HtmlFile},{reason,R},stack])
    end.

get_html(XrzFile) ->
    {ok,FD} = file:open(XrzFile,[read,compressed]),
    try
        {ok,{filename,RL}} = io:read(FD,''),
        escobar_tree:html(escobar_file:get_tree(RL),filename:basename(RL))
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
    escobar_xref:build([Dest || {_,Dest} <- fetch(targets,Conf)]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_xrefs(Conf) ->
    %% we find all {eh}rl files in 'targets', and generate/update an
    %% {eh}rl.xrz file
    Files = up2date(fetch(targets,Conf),"\\.[eh]rl\$",{"",".xrz"}),
    foreach(fun update_xref/1, Files).

update_xref({Targ,Dest}) ->
    io:fwrite("~s...",[Dest]),
    Xrefs = reverse(escobar_mk_xref:go(Targ)),
    {ok,FD} = file:open(Dest,[write,compressed]),
    io:fwrite(FD,"{filename,~p}.~n",[Targ]),
    io:fwrite(FD,"~p.~n",[Xrefs]),
    file:close(FD),
    io:fwrite(" ~p~n",[length(Xrefs)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
up2date(FromTos,RegExp,Exts) ->
    append([up2date(From,RegExp,To,Exts) || {From,To} <- FromTos]).

up2date(From,RegExp,To,Exts) ->
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
conf({target,From,To},Conf) -> append(targets,{chk_dir(From),ass_dir(To)},Conf);
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

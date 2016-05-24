%%%-------------------------------------------------------------------
%%% File    : escobar_xref.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : owns the ets tables. populates the tables from the
%%%               on-disk data base.
%%%
%%% Key is; {Tag,Data,Module}
%%% Data is; [Linenumber]
%%%
%%% imported functions      {{import,{M,F,A},MOD}      ,[LINE]}
%%% exported functions      {{export,{M,F,A},MOD}      ,[LINE]}
%%% included files          {{include,File,MOD}        ,[LINE]}
%%% defined macros          {{macro_def,Macro,MOD}     ,[LINE]}
%%% defined records         {{record_def,Record,MOD}   ,[LINE]}
%%% external function calls {{global_call,{M,F,A},MOD} ,[LINE]}
%%% internal function calls {{local_call,{M,F,A},MOD}  ,[LINE]}
%%% used macros             {{macro,Macro,MOD}         ,[LINE]}
%%% used records            {{record,Record,MOD}       ,[LINE]}
%%%
%%% Created : 17 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(escobar_xref).

-export([build/1,recurse_inc/1,find/1,find/2,find/3]).        % handle xrefs

-import(filelib,[fold_files/5]).
-import(lists,[foreach/2,member/2,append/1]).
-import(filename,[basename/2,basename/1]).

-define(LOG(T),escobar:log(process_info(self()),T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recurse_inc(Inc) ->
    case ets:match(escobar,{{include,'$1',Inc},'_'}) of
        [] -> [Inc];
        Incs -> [Inc|append([recurse_inc(I) || [I] <- Incs])]
    end.

find(Kind,Name,Srcs) ->
    [{File,Lines} || {File,Lines} <- find(Kind,Name), member(File,Srcs)].

find(Kind,Name) ->
    [{F,Ls} || [F,Ls] <- ets:match(escobar,{{Kind,Name,'$1'},'$2'})].

find(Kind) ->
    [{MFA,F,Ls} || [MFA,F,Ls] <- ets:match(escobar,{{Kind,'$1','$2'},'$3'})].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build(Dir) ->
    catch ets:delete(escobar),
    ets:new(escobar,[ordered_set,named_table,public]),
    fold_files(Dir,"\\.xrz$",false,fun ff/2,[]),
    ok.

ff(File,_) ->
    {ok,FD} = file:open(File,[read,compressed]),
    io:read(FD,''),
    {ok,Xrefs} = io:read(FD,''),
    file:close(FD),
    foreach(fun(Xr)-> xrefs(basename(File,".xrz"),Xr) end, Xrefs).

xrefs(MOD,{{import,{M,FAs}},L}) ->     xfa(import,MOD,L,M,FAs);
xrefs(MOD,{{export,FAs},L}) ->         xfa(export,MOD,L,tom(MOD),FAs);
xrefs(MOD,{{include,File},L}) ->       bagit({include,basename(File),MOD},L);
xrefs(MOD,{{macro_def,Macro},L}) ->    bagit({macro_def,Macro,MOD},L);
xrefs(MOD,{{record_def,Record},L}) ->  bagit({record_def,Record,MOD},L);
xrefs(MOD,{{global_call,{M,F,A}},L})-> bagit({global_call,{M,F,A},MOD},L);
xrefs(MOD,{{local_call,{F,A}},L}) ->   bagit({local_call,{tom(MOD),F,A},MOD},L);
xrefs(MOD,{{macro,Macro},L}) ->        bagit({macro,Macro,MOD},L);
xrefs(MOD,{{record,Record},L}) ->      bagit({record,Record,MOD},L).

xfa(Tag,MOD,L,M,FAs) ->
    foreach(fun({F,A}) -> bagit({Tag,{M,F,A},MOD},L) end, FAs).

bagit(Key,Val) ->
    case ets:lookup(escobar,Key) of
        []           -> ets:insert(escobar,{Key,[Val]});
        [{Key,Vals}] -> ets:insert(escobar,{Key,[Val|Vals]})
    end.

tom(Basename) -> list_to_atom(basename(Basename,".erl")).

%%%-------------------------------------------------------------------
%%% File    : escobar_mk_xref.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>

%%% Description : builds the on-disk cross reference data base.  
%%%
%%% Info about each target file is stored in the xref file. 
%%%
%%% the xref file is a gzipped text file. it contains 2 terms.
%%% first term;
%%% {filename,string(Filename)}.
%%% second term;
%%% list(item()).
%%% item() -
%%%   {{import,{M,FAs}}      ,Linenumber}
%%% | {{export,FAs}          ,Linenumber}
%%% | {{include,File}        ,Linenumber}
%%% | {{macro_def,Macro}     ,Linenumber}
%%% | {{record_def,Record}   ,Linenumber}
%%% | {{global_call,{M,F,A}} ,Linenumber}
%%% | {{local_call,{F,A}}    ,Linenumber}
%%% | {{macro,Macro}         ,Linenumber}
%%% | {{record,Record}       ,Linenumber}
%%%
%%% Created : 17 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(escobar_mk_xref).

-include_lib("kernel/include/file.hrl").

-export([go/1]).                                % create xrefs

-define(LOG(T),escobar:log(process_info(self()),T)).

-import(erl_syntax,[type/1,get_pos/1,
		    application_operator/1,application_arguments/1,
		    arity_qualifier_argument/1, arity_qualifier_body/1,
		    atom_value/1,
		    attribute_arguments/1,attribute_name/1,
		    integer_value/1,
		    list_elements/1,
		    macro_name/1,
		    module_qualifier_argument/1, module_qualifier_body/1,
		    record_access_type/1,
		    record_expr_type/1,
		    record_index_expr_type/1,
		    string_value/1,
                    text_string/1,
		    tuple_elements/1,
		    variable_name/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go(File) ->
    do_erl(File).

do_erl(FileName) -> 
    Tree = escobar_file:get_tree(FileName),
    erl_syntax_lib:fold(fun folder/2,[],Tree).

folder(Tree,Acc) ->
    case each(type(Tree),Tree) of
        ok -> Acc;
        {Tag,Data,Pos} -> [{{Tag,Data},Pos}|Acc];
        TDPs when is_list(TDPs) ->
            [{{T,D},P} || {T,D,P} <- lists:reverse(TDPs)]++Acc
    end.

each(text,Tree) ->
    handle_shit(Tree);
each(application,Tree) ->
    h_application(Tree);
each(macro, Tree) ->
    h_macro(Tree);
each(attribute,Tree) ->
    h_attribute(atom_value(attribute_name(Tree)),attribute_arguments(Tree));
each(record_access, Tree) ->
    h_record(record_access_type(Tree));
each(record_expr, Tree) ->
    h_record(record_expr_type(Tree));
each(record_index_expr, Tree) ->
    h_record(record_index_expr_type(Tree));
each(_,_) -> ok.

handle_shit(Tree) ->
    Pos = get_pos(Tree),
    String = text_string(Tree),
    case erl_scan:string(String,Pos) of
        {ok,[{'-',_},{_,_,define},{'(',_},{_,_,Macro}|_],_} -> 
            ?LOG([shitty_macro_def,{macro,Macro}]),
            {macro_def,Macro,Pos};
        {ok,Ts,_} -> 
            try 
                {SM,Form} = or_else([fun()-> remove_shitty_macros(Ts) end]),
                ?LOG([removed_shitty_macro,{macro,SM}]),
                [{macro,SM,Pos}|erl_syntax_lib:fold(fun folder/2,[],Form)]
            catch 
                _:R -> ?LOG([bad_form,{tree,Tree},{reason,R}]),ok
            end
    end.

remove_shitty_macros(Ts) ->
    LL = find_shitty_macro(Ts),
    L = lists:usort(LL),
    Fs = [fun() -> {SM,epp_dodger:normal_parser(remove_shitty_macro(Ts,SM),[])}
          end 
          || SM <- L],
    or_else(Fs).

remove_shitty_macro([{'?',_},{_,_,SM},{'(',_}|R],SM) -> 
    remove_shitty_macro(drop_parens(R,1),SM);
remove_shitty_macro([{'?',_},{_,_,SM}|R],SM) -> remove_shitty_macro(R,SM);
remove_shitty_macro([X|R],SM) -> [X|remove_shitty_macro(R,SM)];
remove_shitty_macro([],_SM) -> [].

find_shitty_macro([{'?',_},{_,_,'MODULE'}|R]) -> find_shitty_macro(R);
find_shitty_macro([{'?',_},{_,_,'LINE'}|R]) -> find_shitty_macro(R);
find_shitty_macro([{'?',_},{_,_,Mac}|R]) -> [Mac|find_shitty_macro(R)];
find_shitty_macro([_|R]) -> find_shitty_macro(R);
find_shitty_macro([]) -> [].

or_else([]) -> throw(no_match);
or_else([F|_]) when not is_function(F) -> exit(bad_fun);
or_else([F|Fs]) when is_function(F) ->
    try F()
    catch _:_ -> or_else(Fs)
    end.

drop_parens([{')',_}|R],1) -> R;
drop_parens([{')',_}|R],N) -> drop_parens(R,N-1);
drop_parens([{'(',_}|R],N) -> drop_parens(R,N+1);
drop_parens([_|R],N) -> drop_parens(R,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% application

h_application(Appl) ->
    Op = application_operator(Appl),
    case appl_op(Op) of
 	ok -> ok;
 	{M,F} -> 
 	    Ari = length(application_arguments(Appl)),
 	    {global_call,{M,F,Ari},get_pos(Appl)};
	F when is_atom(F) ->
	    Ari = length(application_arguments(Appl)),
 	    {local_call,{F,Ari},get_pos(Appl)}
    end.

appl_op(Op) ->
    case type(Op) of
	module_qualifier -> appl_modq(Op);
	tuple -> appl_tup(Op);
	atom -> appl_atom(Op);
	_ -> ok
    end.

appl_atom(X) ->
    atom_value(X).

appl_tup(X) ->
    [ModE,FunE] = tuple_elements(X),
    appl_mf(ModE,FunE).

appl_modq(MQ) ->
    appl_mf(module_qualifier_argument(MQ), module_qualifier_body(MQ)).

appl_mf(Mod,Fun) ->
    case {type(Mod),type(Fun)} of
	{atom,atom} -> {atom_value(Mod),atom_value(Fun)};
	{atom,_} -> {atom_value(Mod),'VAR'};
	{_,atom} -> {'VAR',atom_value(Fun)};
	_ -> ok
    end.
	     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% macro

h_macro(Macro) -> 
    {macro,macro_nam(macro_name(Macro)),get_pos(Macro)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% attributes

h_attribute(Name,Args) ->
    case Name of
 	define -> macro_def(Args);
 	import -> import(Args);
 	export -> export(Args);
 	include -> include(Args);
 	include_lib -> include(Args);
 	record -> record_def(Args);
 	_ -> ok
    end.

macro_def(Args) -> 
    {macro_def, macro_nam(hd(Args)),get_pos(hd(Args))}.

macro_nam(X) ->
    case type(X) of
	variable -> variable_name(X);
	atom -> atom_value(X);
	application -> macro_nam(application_operator(X))
    end.

import([ModTree,AQs]) -> 
    {import, {atom_value(ModTree),aqlist(AQs)},get_pos(ModTree)}.

export([AQlist]) -> 
    {export,aqlist(AQlist),get_pos(AQlist)}.

aqlist(AQs) -> lists:map(fun aqs/1, list_elements(AQs)).
aqs(AQ) -> {atom_value(arity_qualifier_body(AQ)),
	    integer_value(arity_qualifier_argument(AQ))}.

include(Args) ->
    {include, string_value(hd(Args)),get_pos(hd(Args))}.

record_def(Args) ->
    {record_def, atom_value(hd(Args)),get_pos(hd(Args))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% records

h_record(RecType) ->
    case catch type(RecType) of
	atom -> {record, atom_value(RecType), get_pos(RecType)};
	_ -> ok
    end.

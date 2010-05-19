%%%-------------------------------------------------------------------
%%% File    : escobar_file.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created :  6 Jun 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(escobar_file).

-export([get_tree/1]).
%%-export([t2s/1]).

-import(erl_syntax,[set_pos/2,error_marker/1,macro/1,form_list/1,
		    application/2,atom/1,
		    attribute/2,attribute_arguments/1,attribute_name/1,
		    text/1,variable/1]).
-import(lists,[reverse/1,flatten/1]).

-define(LOG(T),escobar:log(process_info(self()),T)).

get_tree(Filename) ->
    case file:read_file_info(Filename) of
	{ok,_} -> 
	    case filename:extension(Filename) of
		".beam" -> get_tree_beam(Filename);
		".erl" -> get_comm_tree_erl(Filename);
		".hrl" -> get_comm_tree_erl(Filename);
		_ -> erlang:error({unknown_filetype,Filename})
	    end;
	_ ->
	    throw(no_file)
    end.

get_comm_tree_erl(Filename) ->
    Comms = erl_comment_scan:file(Filename),
    Forms = get_forms_erl(Filename),
    erl_recomment:recomment_forms(Forms,Comms).

get_tree_beam(Filename) ->
    case beam_lib:chunks(Filename,["Abst"]) of
	{ok,{_,[{"Abst",AChunk}]}} ->
	    {_,Forms} = binary_to_term(AChunk),
	    form_list(Forms);
	_ ->
	    erlang:error({no_debuginfo,Filename})
    end.

get_forms_erl(Filename) ->
    {ok,Fs} = epp_dodger:parse_file(Filename,[{no_fail, true}]),
    Fs.

%% get_forms_erl(Filename) ->
%%     {ok,FD} = file:open(Filename,[read]),
%%     try dodge(FD,1,Filename)
%%     after file:close(FD)
%%     end.

%% dodge(FD,Line,FN) ->
%%     case io:scan_erl_form(FD, "", Line) of
%% 	{ok, Ts, L1} ->
%% 	    [make_form(Ts,L1,FN)|dodge(FD,L1,FN)];
%% 	{eof, _LineNo} -> 
%%  	    [];
%%  	Err ->
%%  	    erlang:error({scanner_failed,FN,Line,Err})
%%     end.

%% %%% a whole bloody section to deal with "non-syntactic" macros, i.e. the
%% %%% ones that the erlang parser chokes on
%% %%% we try to run a bunch of hacks (the hax_* funs) on the tokens.
%% %%% if they all fail, just turn the tokens into text.

%% make_form(Toks, Line, File) ->
%%     Hax = [fun() -> epp_dodger:normal_parser(Toks,[]) end,
%% 	   fun() -> hax_non_syntactic_def(Toks) end,
%% 	   fun() -> hax_missing_closing_paren(Toks) end,
%% 	   fun() -> hax_hash_in_macro(Toks) end],
%%     try or_else(Hax) of
%% 	{Hack,Form} -> ?LOG([hack,{file,File},{hack,Hack},{line,Line}]), Form;
%% 	Form -> Form
%%     catch 
%% 	throw:no_match ->
%%             try 
%%                 %%Str = epp_dodger:tokens_to_string(Toks),
%%                 Str = t2s(Toks),
%%                 ?LOG([{file,File},{line,Line},{str,Str}]),
%%                 set_pos_r(text(Str),Line)
%%             catch
%%                 _:R -> 
%%                     ?LOG([crash_form,{file,File},{line,Line},{reason,R}]),
%%                     set_pos_r(text("ERROR"),Line)
%%             end
%%     end.

%% or_else([]) -> throw(no_match);
%% or_else([F|_]) when not is_function(F) -> exit(bad_fun);
%% or_else([F|Fs]) when is_function(F) ->
%%     try F()
%%     catch _:_ -> or_else(Fs)
%%     end.

%% hax_hash_in_macro(Toks) ->
%%     %% a macro that starts with #, e.g. -define(foo,#foo).
%%     {hax_hash_in_macro,hax_hash_in_macro(Toks,[])}.

%% hax_hash_in_macro([{'?',L},Mac,{'.',L},{atom,L,Atom}|Toks],O) -> 
%%     hax_hash_in_macro(Toks,[{atom,L,Atom},{'.',L},Mac,{'?',L},{'#',L}|O]);
%% hax_hash_in_macro([{'?',L},Mac,{'{',L}|Toks],O) -> 
%%     hax_hash_in_macro(Toks,[{'{',L},Mac,{'?',L},{'#',L}|O]);
%% hax_hash_in_macro([H|Toks],O) ->
%%     hax_hash_in_macro(Toks,[H|O]);
%% hax_hash_in_macro([],O) ->
%%     epp_dodger:normal_parser(reverse(O),[]).

%% hax_missing_closing_paren(Toks) ->
%%     %% a macro def without closing paren (why is this allowed???)
%%     [{dot,Line}|Head] = reverse(Toks),
%%     {hax_missing_closing_paren,
%%      epp_dodger:normal_parser(reverse([{dot,Line},{')',Line}|Head]),[])}.

%% hax_non_syntactic_def(Toks) ->
%%     %% defining a macro to something non-syntactic, e.g.;
%%     %% -define(X(Y), Y a).
%%     [{'-',_},{atom,_,define},{'(',_},_|R1] = Toks,
%%     TakeF = fun({')',_}) -> false; (_) -> true end,
%%     case R1 of
%% 	[{'(',_}|_] -> ArgL = length(lists:takewhile(TakeF,R1))+6;
%% 	[{',',_}|_] -> ArgL = 5
%%     end,
%%     {ToksH,X} = lists:split(ArgL,Toks),
%%     {ToksM,ToksT} = lists:split(length(X)-2,X),
%%     PreForm = epp_dodger:normal_parser(ToksH++[{var,0,'X'}|ToksT],[]),
%%     Text = text(epp_dodger:tokens_to_text(ToksM)),
%%     {hax_non_syntactic_def,
%%      attribute(attribute_name(PreForm),
%% 	       [hd(attribute_arguments(PreForm)),Text])}.

%% set_pos_r(Tree,Pos) -> erl_syntax_lib:map(fun(N)->set_pos(N,Pos) end, Tree).

%% t2s([])                -> [];
%% t2s([{dot,_}])         -> ["."];
%% t2s([{string,_,S}|T])  -> [$ ,$",S,$"|t2s(T)];
%% t2s([{float,_,F}|T])   -> [$ ,float_to_list(F)|t2s(T)];
%% t2s([{integer,_,I}|T]) -> [$ ,integer_to_list(I)|t2s(T)];
%% t2s([{char,_,C}|T])    -> [$ ,C|t2s(T)];
%% t2s([{_,_,A}|T])       -> [$ ,atom_to_list(A)|t2s(T)];
%% t2s([{A,_}|T])         -> [$ ,atom_to_list(A)|t2s(T)].

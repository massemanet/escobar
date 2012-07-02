%%%-------------------------------------------------------------------
%%% File    : escobar_file.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created :  6 Jun 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(escobar_file).

-export([get_tree/1]).

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
	    erl_syntax:form_list(Forms);
	_ ->
	    erlang:error({no_debuginfo,Filename})
    end.

get_forms_erl(Filename) ->
    {ok,Fs} = epp_dodger:parse_file(Filename,[{no_fail, true}]),
    Fs.

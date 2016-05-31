%%%-------------------------------------------------------------------
%%% File    : escobar_tree.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description :
%%%
%%% Created :  6 Jun 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(escobar_tree).

-export([html/2]).

-define(LOG(T),escobar:log(process_info(self()),T)).

-import(erl_syntax,
        [get_ann/1,set_ann/2,add_ann/2,
         subtrees/1,update_tree/2,type/1,get_pos/1,
         application/2,application_arguments/1,application_operator/1,
         arity_qualifier_argument/1,arity_qualifier_body/1,
         atom_name/1,atom_value/1,
         attribute/2,attribute_name/1,attribute_arguments/1,
         comment/2,comment_text/1,comment_padding/1,
         function/2,function_name/1,function_clauses/1,function_arity/1,
         integer_literal/1,
         list/1,list_elements/1,
         macro/2,macro_name/1,macro_arguments/1,
         module_qualifier_body/1, module_qualifier_argument/1,
         string/1,string_value/1,  %string_literal/1,
         variable_name/1,          %variable_literal/1,
         record_access/3, record_access_argument/1,
         record_access_field/1,record_access_type/1,
         record_expr/3, record_expr_argument/1,
         record_expr_fields/1, record_expr_type/1,
         record_index_expr/2, record_index_expr_field/1,
         record_index_expr_type/1,
         get_precomments/1,get_postcomments/1,has_comments/1,
         copy_comments/2,remove_comments/1]).

-import(prettypr,
        [above/2,beside/2,empty/0,
         null_text/1,break/1,floating/3,text/1,floating/1]).

-import(lists,[flatten/1,duplicate/2,keysearch/3,member/2,usort/1]).

-import(filename,[join/1,basename/1]).

%%% # html
%%% turn a syntax tree into html by annotating and pretty-printing
%%% with a hook function

html(Tree,Basename) ->
  clear_cache(),
  put_cache({basename,Basename}),
  pout(ann(type(Tree),Tree)).

%%lists:foldl(fun(Form,Acc) -> [pout(ann(Form))|Acc] end, [], Tree).

pout(Form) ->
  erl_prettypr:format(Form,[{hook,fun tag/3},{paper,80},{ribbon,65}]).

%%% ## formatting
%%% ### 'tag' - the format hook function
tag(Node,Ctxt,Cont) ->
  Tags = get_ann(Node),
  case member(has_comment,Tags) of
    true ->
      PreC = get_precomments(Node),
      PostC = get_postcomments(Node),
      Nod = remove_comments(Node),
      Doc0 = tagit(Tags--[has_comment],Cont(Nod,Ctxt)),
      postcomment(precomment(Doc0,PreC),PostC);
    false ->
      Doc0 = Cont(Node,Ctxt),
      tagit(Tags,Doc0)
  end.

tagit([],Doc0) -> Doc0;
tagit([{Beg,End}],Doc0) -> beside(null_text(Beg),beside(Doc0,null_text(End)));
tagit(["binary"],{beside,_,{beside,Doc,_}}) ->
  beside(floating(text("&lt;&lt;")),beside(Doc,floating(text("&gt;&gt;")))).

%%%### comment stuff
precomment(Doc,PreC) ->
  above(floating(break(stack(PreC)), -1, -1), Doc).
postcomment(Doc,PostC) ->
  beside(Doc, floating(break(stack(PostC)), 1, 0)).

stack([]) -> empty();
stack([Comm|Comms]) ->
  Doc = maybe_pad(stack_comment_lines(comment_text(Comm)),Comm),
  case Comms of
    [] -> Doc;
    _ -> above(Doc, stack(Comms))
  end.

maybe_pad(Doc,Comm) ->
  case comment_padding(Comm) of
    I when is_integer(I), 0 < I -> beside(text(duplicate(I,$ )), Doc);
    _ -> Doc
  end.

%%% stolen with pride from erl_prettypr
%%% Stack lines of text above each other and prefix each string in
%%% the list with a single `%' character.

stack_comment_lines([S | Ss]) ->
  D = tagit([dehtml('span', [{class,comment}])],text("%"++debracket(S))),
  case Ss of
    [] -> D;
    _ -> above(D, stack_comment_lines(Ss))
  end;
stack_comment_lines([]) ->
  empty().

%%% annotate nodes that should be hilited
%%% the annotation is put on the subtree that should be marked up
%%% the annotation is;
%%% has_comments|{BegMarkup,EndMarkup}
%%% if a node already has an annotation the new one is dropped, except
%%% if either the new or the old one is has_comments

ann(binary,Tree) ->
  new_tree(Tree,add_anno("binary",Tree));
ann(attribute,Tree) ->
  Name = attribute_name(Tree),
  Args =
  case atom_value(Name) of
    export ->
      AQs = list_elements(hd(attribute_arguments(Tree))),
      [list([add_anno(mu(export,AQ),AQ) || AQ <- AQs])];
    define ->
      [Macro|Rest] = attribute_arguments(Tree),
      put_cache({{macro_def,safe_name(Macro)},"#mac-"++str_name(Macro)}),
      case type(Macro) of
        application ->
          Op = application_operator(Macro),
          As = application_arguments(Macro),
          [application(add_anno(mu(define,Op),Op),As)|Rest];
        _ ->
          [add_anno(mu(define,Macro),Macro)|Rest]
      end;
    record ->
      [Rec|Rest] = attribute_arguments(Tree),
      put_cache({{record_def,safe_name(Rec)},"#rec-"++str_name(Rec)}),
      [add_anno(mu(record,Rec),Rec)|Rest];
    include ->
      [IncTree] = attribute_arguments(Tree),
      Inc = basename(string_value(IncTree)),
      OIncs = get_cache(includes),
      Incs = escobar_xref:recurse_inc(Inc),
      put_cache({includes,usort(Incs++OIncs)}),
      [add_anno(mu(include,IncTree),IncTree)];
    include_lib ->
      [IncTree] = attribute_arguments(Tree),
      Inc = string_value(IncTree),
      OIncs = get_cache(includes),
      Incs = escobar_xref:recurse_inc(Inc),
      put_cache({includes,usort(Incs++OIncs)}),
      [add_anno(mu(include,IncTree),IncTree)];
    _ ->
      attribute_arguments(Tree)
  end,
  new_tree(Tree,attribute(Name,Args));
ann(record_access,Tree) ->
  Arg = record_access_argument(Tree),
  Type = record_access_type(Tree),
  Field = record_access_field(Tree),
  new_tree(Tree,record_access(Arg,add_anno(mu(rec_acc,Tree),Type),Field));
ann(record_expr,Tree) ->
  Arg = record_expr_argument(Tree),
  Type = record_expr_type(Tree),
  Fields = record_expr_fields(Tree),
  new_tree(Tree,record_expr(Arg,add_anno(mu(rec_expr,Tree),Type),Fields));
ann(record_index_expr,Tree) ->
  Type = record_index_expr_type(Tree),
  Field = record_index_expr_field(Tree),
  new_tree(Tree,record_index_expr(add_anno(mu(rec_iexpr,Tree),Type),Field));
ann(function,Tree) ->
  Name = function_name(Tree),
  Clauses = function_clauses(Tree),
  new_tree(Tree,function(add_anno(mu(function,Tree),Name),Clauses));
ann(application,Tree) ->
  Op = application_operator(Tree),
  Args = application_arguments(Tree),
  new_tree(Tree,application(add_anno(mu(application,Tree),Op),Args));
ann(macro,Tree) ->
  Name = macro_name(Tree),
  Args = macro_arguments(Tree),
  new_tree(Tree,macro(add_anno(mu(macro,Tree),Name), Args));
ann(string,OTree) ->
  Tree = set_ann(string(debracket(string_value(OTree))),get_ann(OTree)),
  new_tree(OTree,add_anno(mu(string),Tree));
ann(variable,Tree) ->
  new_tree(Tree,add_anno(mu(variable),Tree));
ann(comment,OTree) ->
  Pad = comment_padding(OTree),
  Text = [debracket(S) || S <- comment_text(OTree)],
  Tree = comment(Pad,Text),
  new_tree(OTree,add_anno(mu(comment),Tree));
ann(error_marker,Tree) ->
  new_tree(Tree,add_anno(mu(error_marker),Tree));
ann(_Typ,Tree) ->
  new_tree(Tree,Tree).

new_tree(OTree,NTree) ->
  Tree =
    case has_comments(OTree) of
      true -> add_ann(has_comment,copy_comments(OTree,NTree));
      false -> NTree
    end,
  SubTrees = subtrees(Tree),
  case [[ann(type(SubT),SubT) || SubT<-Group] || Group<-SubTrees] of
    [] -> Tree;
    NSubtrees -> update_tree(Tree,NSubtrees)
  end.

debracket([]) -> [];
debracket([$>|Str]) -> "&gt;"++debracket(Str);
debracket([$<|Str]) -> "&lt;"++debracket(Str);
debracket([C|Str]) -> [C|debracket(Str)].

add_anno(nil,Tree) -> Tree;
add_anno(Ann,Tree) ->
  case get_ann(Tree) of
    [] -> add_ann(Ann,Tree);
    [has_comment] -> add_ann(Ann,Tree);
    _OAnn -> Tree
  end.

%%%### the markups
mu(comment) -> dehtml('span', [{class,comment}]);
mu(string) -> dehtml('span', [{class,string}]);
mu(variable) -> dehtml('span', [{class,variable}]);
mu(error_marker) -> dehtml('span', [{class,error_marker}]).

mu(function,Node) ->
  M = hd(string:tokens(get_cache(basename),".")),
  F = atom_name(function_name(Node)),
  A = str(function_arity(Node)),
  dehtml('a', [{class,function},
               {href,"xrefs.html#"++M++":"++F++"/"++A},
               {name,F++"/"++A}]);
mu(include,Inc) ->
  File = string_value(Inc),
  dehtml('a', [{class,include},
               {href,string:join(string:tokens(File,"/"),".")++".html"}]);
mu(export,AQ) ->
  M = atom_name(arity_qualifier_body(AQ)),
  A = integer_literal(arity_qualifier_argument(AQ)),
  dehtml('a', [{href,"#"++M++"/"++A},{class,export}]);
mu(define,Macro) ->
  dehtml('a', [{class,macro},{name,"mac-"++str_name(Macro)}]);
mu(record,Rec) ->
  dehtml('a', [{class,record},{name,"rec-"++str_name(Rec)}]);
mu(rec_acc,Node) ->
  Name = atom_value(record_access_type(Node)),
  dehtml('a', [{href,href(record_def,"#rec-",Name)},{class,record}]);
mu(rec_expr,Node) ->
  Name = atom_value(record_expr_type(Node)),
  dehtml('a', [{href,href(record_def,"#rec-",Name)},{class,record}]);
mu(rec_iexpr, Node) ->
  Name = atom_value(record_index_expr_type(Node)),
  dehtml('a', [{href,href(record_def,"#rec-",Name)},{class,record}]);
mu(macro,Node) ->
  case Name = safe_name(Node) of
    _ when Name=='MODULE'; Name=='LINE' ->
      dehtml('span', [{class,macro}]);
    _ ->
      dehtml('a', [{href,href(macro_def,"#mac-",Name)},{class,macro}])
  end;
mu(application,Node) ->
  Op = application_operator(Node),
  Ar = str(length(application_arguments(Node))),
  case type(Op) of
    variable ->
      dehtml('span', [{class,variable}]);
    atom ->
      Basename = get_cache(basename),
      MFABLs = escobar_xref:find(import),
      Fu = atom_name(Op),
      Fun = list_to_atom(Fu),
      Ari = list_to_integer(Ar),
      case [M || {{M,F,A},B,_} <- MFABLs,F==Fun,A==Ari,B==Basename] of
        [] ->
          dehtml('a', [{href,"#"++join([Fu,Ar])},
                       {name,str(get_pos(Op))},
                       {class,function}]);
        [Mod] ->
          Mo = atom_to_list(Mod),
          Ref = external_call(Mo,Fu,Ar),
          dehtml('a', [{href,Ref},
                       {name,str(get_pos(Op))},
                       {class,function}])
      end;
    module_qualifier ->
      Mod = module_qualifier_argument(Op),
      Fun = module_qualifier_body(Op),
      case {type(Mod),type(Fun)} of
        {atom,atom} ->
          M = atom_name(Mod),
          F = atom_name(Fun),
          Ref = external_call(M,F,Ar),
          dehtml(a, [{href,Ref},
                     {name,str(get_pos(Op))},
                     {class,function}]);
        _ ->
          nil
      end;
    fun_expr ->
      nil;
    _ ->
      ?LOG({bad_application_operator,[{node,Node}]}),
      nil
  end;
mu(X,_Node) ->
  erlang:error({bad_type,X}).

external_call(M,F,A) ->
  case ets:lookup(escobar,{otp,M,F++"-"++A}) of
    []  -> M++".erl.html#"++F++"/"++A;
    [_] -> "http://erlang.org/doc/man/"++M++".html#"++F++"-"++A
  end.

str_name(X) -> str(safe_name(X)).

safe_name(X) ->
  case type(X) of
    variable -> variable_name(X);
    atom -> atom_value(X);
    macro -> safe_name(macro_name(X));
    application -> safe_name(application_operator(X))
  end.

href(Tag,Anch,Name) ->
  case get_cache({Tag,Name}) of
    [] ->
      Basename = get_cache(basename),
      Srcs = [Basename|[dotted_name(I) || I <- get_cache(includes)]],
      case escobar_xref:find(Tag,Name,Srcs) of
        Fs when 1 < length(Fs) ->
          ?LOG([multi_def,[{item,Name},{file,Basename},{files,Fs}]]),
          nil;
        [{File,_}] ->
          File++".html"++Anch++str(Name);
        [] ->
          ?LOG({no_define,[{item,Name},{file,Basename},{srcs,Srcs}]}),
          nil
      end;
    Mcr -> Mcr
  end.

dotted_name(Filename) ->
  string:join(string:tokens(Filename,"/"),".").

dehtml(Tag,Atts) ->
  {flatten([$<,str(Tag),$ ,[[str(A),"=\"",str(V),"\" "]||{A,V}<-Atts],$>]),
  flatten(["</",str(Tag),$>])}.

str(I) when is_integer(I) -> integer_to_list(I);
str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cache is implemented as a list of tagged tuples in the process registry.
%%% silliest possible approach, but probably fast enough.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_cache(Tag) ->
  case keysearch(Tag,1,get(cache)) of
    {value,{Tag,Val}} -> Val;
    false -> []
  end.

clear_cache() ->
  put(cache,[]).

put_cache({Tag,Val}) ->
  case catch get_cache(Tag) of
    [] -> put(cache, [{Tag,Val}|get(cache)]);
    OVal -> put(cache,[{Tag,Val}|get(cache)--[{Tag,OVal}]])
  end.

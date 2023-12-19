:- initialization main, halt.
:- consult("../common.pro").


parse_rule(Str, Rule) :-
    % a<2006:qkq -> (a,<,2006,"qkq")
    split_string(Str, ":", "", [CondStr, Target]), 
    % ^ only works if there is a single ":" in the string
    string_chars(CondStr, [Var,Op|ValStr]),
    string_chars(Val, ValStr),
    number_string(ValNum, Val),
    Rule = (Var,Op,ValNum,Target).
parse_rule(Str, Rule) :-
    % there is no ":" in the string
    Rule = (else,_,_,Str).

parse_workflow(Str, Name-RuleList) :-
    % Str = "px{a<2006:qkq,m>2090:A,rfg}"
    % Name -> "px": RuleList -> [(a,<,2006,"qkq"),(m,>,2090,"A"),(else,_,_,"rfg")]
    split_string(Str, "{", "", [Name, RuleStrWithTail]),
    string_concat(RuleStr, "}", RuleStrWithTail),
    split_string(RuleStr, ",", "", RuleStrList),
    maplist(parse_rule, RuleStrList, RuleList).

% fill_workflowAssoc(StrList, WorkflowAssoc, FinalWorkflowAssoc)
fill_workflowAssoc([], WFAssoc, WFAssoc).
fill_workflowAssoc([Str|StrList], WFAssoc, FinalWFAssoc) :-
    parse_workflow(Str, Name-RuleList),
    put_assoc(Name, WFAssoc, RuleList, NewWFAssoc),
    fill_workflowAssoc(StrList, NewWFAssoc, FinalWFAssoc).
fill_workflowAssoc(StrList, WFAssoc) :-
    empty_assoc(EmptyWFAssoc),
    fill_workflowAssoc(StrList, EmptyWFAssoc, WFAssoc).

parse_part(Str, Part) :-
    % Str = "{x=787,m=2655,a=1222,s=2876}" -> Part = part{x:787,m:2655,a:1222,s:2876}
    string_concat("{", PartStrWithTail, Str),
    string_concat(PartStr, "}", PartStrWithTail),
    split_string(PartStr, ",", "", PartStrList),
    maplist([X,Y]>>(
        split_string(X, "=", "", [_,ValStr]), 
        number_string(Y, ValStr)
    ), PartStrList, L),
    L = [X,M,A,S],
    Part = part{x:X,m:M,a:A,s:S}.

parse_lines(Lines, WFAssoc-PartList) :-
    append([WorkflowStrList,[''],PartStrList], Lines),
    fill_workflowAssoc(WorkflowStrList, WFAssoc),!,
    maplist(parse_part, PartStrList, PartList),!.

% step_part_rulelist(Part, RuleList, Target) :-
step_part_rulelist(_, [LastRule], Target) :-
    LastRule = (else,_,_,Target).
step_part_rulelist(Part, [Rule|Rs], Target) :-
    Rule = (Var,Op,Val,RuleTarget),
    % LHS = Part.Var,
    (compare(Op, Part.Var, Val) -> Target = RuleTarget
    ; step_part_rulelist(Part, Rs, Target)).

step_part(Part, Pos, WFAssoc, Target) :-
    get_assoc(Pos, WFAssoc, RuleList),
    step_part_rulelist(Part, RuleList, Target).

% organize_part(Part, Pos, WFassoc, Result) :-
organize_part(_, "A", _, "A").
organize_part(_, "R", _, "R").
organize_part(Part, Pos, WFassoc, Result) :-
    step_part(Part, Pos, WFassoc, Target),
    organize_part(Part, Target, WFassoc, Result).
organize_part(Part, WFassoc, Result) :- 
    organize_part(Part, "in", WFassoc, Result).

part_rating(Part, Rating) :- Rating is Part.x + Part.m + Part.a + Part.s.
part_rating(Part, "A", Rating) :- part_rating(Part, Rating).
part_rating(_, "R", 0).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl, nl,
    parse_lines(Lines, WFAssoc-PartList),
    % write(WFAssoc), nl, nl,
    % write(PartList), nl,
    map3(organize_part, PartList, WFAssoc, ResultList),
    write(ResultList), nl,
    maplist(part_rating, PartList, ResultList, RatingList),
    sum_list(RatingList, Rating),
    write(Rating),
    nl.

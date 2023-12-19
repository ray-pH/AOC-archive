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

parse_lines(Lines, WFAssoc) :-
    append([WorkflowStrList,[''],_], Lines),
    fill_workflowAssoc(WorkflowStrList, WFAssoc).

split_part(Part, Rule, [CorrectPart, RestPart]) :-
    Part = Pos-Data,
    Rule = (Var,Op,Val,Target),
    Data.Var = (PartMin, PartMax),
    %      A---|---B
    %   |  A-------B
    %      A-------B   |
    (Op = < ->
        % (1,4000) [< 2006] -> (1,2005),(2006,4000)
        CorrectMin = PartMin,
        CorrectMax is min(Val-1, PartMax),
        RestMin is max(Val, PartMin),
        RestMax = PartMax
    ; Op = > ->
        % (1,4000) [> 2006] -> (1,2006),(2007,4000)
        CorrectMin is max(Val+1, PartMin),
        CorrectMax = PartMax,
        RestMin = PartMin,
        RestMax is min(Val, PartMax)
    ),!,
    CorrectData = Data.put(Var, (CorrectMin, CorrectMax)),
    RestData = Data.put(Var, (RestMin, RestMax)),
    CorrectPart = Target-CorrectData,
    RestPart = Pos-RestData.

% step_part_rulelist(Part, RuleList, NextParts) :-
step_part_rulelist(Part, [LastRule], NextParts) :-
    Part = _-Data,
    LastRule = (else,_,_,Target),!,
    NextParts = [Target-Data].
step_part_rulelist(Part, [Rule|Rs], NextParts) :-
    % (a,<,2006,"qkq")
    split_part(Part, Rule, [CorrectPart, RestPart]),!,
    NextParts = [CorrectPart|RestNextParts],
    step_part_rulelist(RestPart, Rs, RestNextParts).

valid_range((Min,Max)) :- Min =< Max.
valid_part(Part) :-
    Part = _-Data,
    valid_range(Data.x),
    valid_range(Data.m),
    valid_range(Data.a),
    valid_range(Data.s).

step_part(Part, WFAssoc, NextParts) :-
    Part = Pos-_,
    get_assoc(Pos, WFAssoc, RuleList),
    step_part_rulelist(Part, RuleList, NextParts).
step_parts(Parts, WFAssoc, NextParts) :-
    map3(step_part, Parts, WFAssoc, NextPartsList),
    flatten1(NextPartsList, NextPartsUnvalidated),
    include(valid_part, NextPartsUnvalidated, NextParts).

rejected_part("R"-_).
accepted_part("A"-_).
progress_part(Part) :- \+ rejected_part(Part), \+ accepted_part(Part).

% organize_parts(Parts, WFAssoc, Results).
organize_parts([], _, ResultsAcc, ResultsAcc).
organize_parts(Parts, WFAssoc, ResultsAcc, FinalResults) :-
    step_parts(Parts, WFAssoc, NextParts),
    include(accepted_part, NextParts, AcceptedParts),
    include(progress_part, NextParts, ProgressParts),
    append(ResultsAcc, AcceptedParts, NewResultsAcc),
    organize_parts(ProgressParts, WFAssoc, NewResultsAcc, FinalResults).
organize_parts(Parts, WFAssoc, FinalResults) :-
    organize_parts(Parts, WFAssoc, [], FinalResults).

count_possibility(Part, Possibility) :-
    Part = _-Data,
    Data.x = (XMin, XMax),
    Data.m = (MMin, MMax),
    Data.a = (AMin, AMax),
    Data.s = (SMin, SMax),
    Possibility is (XMax-XMin+1)*(MMax-MMin+1)*(AMax-AMin+1)*(SMax-SMin+1).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl, nl,
    parse_lines(Lines, WFAssoc),!,
    % write(WFAssoc), nl, nl,
    InitialData = part{x:(1,4000), m:(1,4000), a:(1,4000), s:(1,4000)},
    Parts = ["in"-InitialData],
    % step_parts(Parts, WFAssoc, NextParts),
    % write(NextParts), nl, nl,
    organize_parts(Parts, WFAssoc, Results),
    % write(Results), nl, nl,
    % maplist(writeln, Results),
    maplist(count_possibility, Results, Possibilities),
    sum_list(Possibilities, Sum),
    write(Sum),
    nl.

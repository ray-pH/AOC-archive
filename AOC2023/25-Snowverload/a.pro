:- initialization main, halt.
:- consult("../common.pro").

% crj: tml qvk zvj jcb
parse_line(Line, Name-Targets) :-
    split_string(Line, ":", " ", [Name,TargetsStr]),
    split_string(TargetsStr, " ", " ", Targets).

add_edge(Name, Target, Graph0, Graph) :-
    % if name is in graph, add target to its targets
    get_assoc(Name, Graph0, NameTargets),
    (member(Target, NameTargets) ->
        Graph = Graph0
    ;
        put_assoc(Name, Graph0, [Target|NameTargets], Graph)
    ).
add_edge(Name, Target, Graph0, Graph) :-
    % if name is not in graph, add it with target as its target
    \+ get_assoc(Name, Graph0, _),
    put_assoc(Name, Graph0, [Target], Graph).

add_edge2(Name, Target, Graph0, Graph) :-
    add_edge(Name, Target, Graph0, Graph1),
    add_edge(Target, Name, Graph1, Graph).

add_nametargets(Name-Targets, Graph0, Graph) :-
    foldl(add_edge2(Name), Targets, Graph0, Graph).

generate_graph(ParsedLines, Graph) :-
    foldl(add_nametargets, ParsedLines, t, Graph).

remove_edge(Name, Target, Graph0, Graph) :-
    get_assoc(Name, Graph0, NameTargets),
    delete(NameTargets, Target, NameTargets1),
    put_assoc(Name, Graph0, NameTargets1, Graph).
remove_edge2(Name, Target, Graph0, Graph) :-
    remove_edge(Name, Target, Graph0, Graph1),
    remove_edge(Target, Name, Graph1, Graph).

get_assocl(Graph, Name, Targets) :- get_assoc(Name, Graph, Targets).
memberof(Arr, Elem) :- member(Elem, Arr).
traverse_graph([], _, Visited, Visited).
traverse_graph(Currents, Graph, Visited, FinalVisited) :-
    append(Currents, Visited, NewVisited),
    maplist(get_assocl(Graph), Currents, CurrentsTargets),
    flatten1(CurrentsTargets, CurrentsTargetsFlat),
    unique(CurrentsTargetsFlat, CurrentsTargetsFlatUnique),
    exclude(memberof(NewVisited), CurrentsTargetsFlatUnique, NewCurrents),
    traverse_graph(NewCurrents, Graph, NewVisited, FinalVisited).


main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(parse_line, Lines, ParsedLines),
    % write(ParsedLines),
    generate_graph(ParsedLines, Graph0),
    % example
    % remove_edge2("hfx", "pzl", Graph0, Graph1),
    % remove_edge2("bvb", "cmg", Graph1, Graph2),
    % remove_edge2("nvd", "jqt", Graph2, Graph),
    % NodeA = "hfx",
    % NodeB = "pzl",
    % input
    remove_edge2("prk", "gpz", Graph0, Graph1),
    remove_edge2("zhg", "qdv", Graph1, Graph2),
    remove_edge2("rfq", "lsk", Graph2, Graph),
    NodeA = "prk",
    NodeB = "gpz",
    %
    traverse_graph([NodeA], Graph, [], VisitedA),
    length(VisitedA, LengthA), write(LengthA), nl,
    traverse_graph([NodeB], Graph, [], VisitedB),
    length(VisitedB, LengthB), write(LengthB), nl,
    Product is LengthA * LengthB,
    write(Product),
    nl.

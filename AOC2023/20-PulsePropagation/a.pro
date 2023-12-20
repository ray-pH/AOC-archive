:- initialization main, halt.
:- consult("../common.pro").

char_type('%', flipflop).
char_type('&', conjunction).
char_type('b', broadcast).

%  %gb -> xf, kr
%  &tz -> cn, fk, ls
%  broadcaster -> sj, pf, kh, cn
%  Module = m{name, type, dest, state}
parse_module(ModuleStr, Module) :-
    split_string(ModuleStr, " ", ",-> ", [NameStr|DestNameList]),
    string_chars(NameStr, [TypeChar|NameChars]),
    char_type(TypeChar, Type),
    (Type = broadcast -> Name = "broadcaster"; string_chars(Name, NameChars)),
    Module = m{name:Name, type:Type, dests:DestNameList, state:t}.

module_update_assoc(Module, Assoc, UpdatedAssoc) :-
    put_assoc(Module.name, Assoc, Module, UpdatedAssoc).

% modules_to_assoc(Modules, AssocAcc, FinalAssoc) :-
modules_assoc([], AssocAcc, AssocAcc).
modules_assoc([M|Ms], AssocAcc, FinalAssoc) :-
    module_update_assoc(M, AssocAcc, UpdatedAssoc),
    modules_assoc(Ms, UpdatedAssoc, FinalAssoc).
modules_assoc(Modules, Assoc) :-
    modules_assoc(Modules, t, Assoc).

% update_moduleassoc(ModuleNames, AssocAcc, FinalAssoc)
update_moduleassoc([], FinalAssoc, FinalAssoc).
update_moduleassoc([N|Ns], Assoc, FinalAssoc) :-
    get_assoc(N, Assoc, Module),
    %
    (Module.type = flipflop ->
        % if flipflop, add (N,0) to state
        put_assoc(N, Module.state, 0, UpdatedState),
        NewModule = Module.put(state, UpdatedState),
        put_assoc(N, Assoc, NewModule, UpdatedAssoc)
    ;
        UpdatedAssoc = Assoc
    ),
    update_conjassoc(N, Module.dests, UpdatedAssoc, UpdatedAssoc2),
    update_moduleassoc(Ns, UpdatedAssoc2, FinalAssoc).

update_conjassoc_single(_, ConjName, Assoc, NextAssoc) :-
    % if not conjunction, do nothing
    get_assoc(ConjName, Assoc, ConjModule),
    ConjModule.type \= conjunction,
    Assoc = NextAssoc.
update_conjassoc_single(InputName, ConjName, Assoc, NextAssoc) :-
    % if conjunction, add input to state and set it to 0
    get_assoc(ConjName, Assoc, ConjModule),
    ConjModule.type = conjunction,
    put_assoc(InputName, ConjModule.state, 0, UpdatedState),
    NewModule = ConjModule.put(state, UpdatedState),
    put_assoc(ConjName, Assoc, NewModule, NextAssoc).
update_conjassoc_single(_, ConjName, Assoc, NextAssoc) :-
    % name is not in assoc, add it
    \+ get_assoc(ConjName, Assoc, _),
    Module = m{name:ConjName, type:broadcast, dests:[], state:t},
    put_assoc(ConjName, Assoc, Module, NextAssoc).

update_conjassoc(_, [], Assoc, Assoc).
update_conjassoc(InputName, [ConjName|ConjNames], Assoc, FinalAssoc) :-
    update_conjassoc_single(InputName, ConjName, Assoc, UpdatedAssoc),
    update_conjassoc(InputName, ConjNames, UpdatedAssoc, FinalAssoc).

flip(0,1).
flip(1,0).
% Pulse = p{target, level, source}
send_pulse(Pulse, ModAssoc, NewPulseList, NewModAssoc) :-
    % module is broadcast
    get_assoc(Pulse.target, ModAssoc, Module),
    Module.type = broadcast,!,
    NewModAssoc = ModAssoc,
    maplist([X,Y]>>(Y = p{target:X, level:Pulse.level, source:Module.name}), Module.dests, NewPulseList).
send_pulse(Pulse, ModAssoc, NewPulseList, NewModAssoc) :-
    % module is flipflop
    % if pulse level is 1, nothing happen
    get_assoc(Pulse.target, ModAssoc, Module),
    Module.type = flipflop,
    Pulse.level = 1,
    NewPulseList = [],
    NewModAssoc = ModAssoc.
send_pulse(Pulse, ModAssoc, NewPulseList, NewModAssoc) :-
    % module is flipflop
    % if pulse level is 0, flip the state
    % ModuleName = Pulse.target,
    get_assoc(Pulse.target, ModAssoc, Module),
    Module.type = flipflop,
    Pulse.level = 0,
    % SelfState
    get_assoc(Module.name, Module.state, SelfState),
    flip(SelfState, NewState),
    put_assoc(Module.name, Module.state, NewState, UpdatedState),
    NewModule = Module.put(state, UpdatedState),
    put_assoc(Module.name, ModAssoc, NewModule, NewModAssoc),
    maplist([X,Y]>>(Y = p{target:X, level:NewState, source:Module.name}), Module.dests, NewPulseList).
send_pulse(Pulse, ModAssoc, NewPulseList, NewModAssoc) :-
    % module is conjunction
    get_assoc(Pulse.target, ModAssoc, Module),
    Module.type = conjunction,
    % update the state first
    put_assoc(Pulse.source, Module.state, Pulse.level, UpdatedState),
    NewModule = Module.put(state, UpdatedState),
    put_assoc(Module.name, ModAssoc, NewModule, NewModAssoc),
    % if all state is 1, send 0; else, send 1
    (conjstate_all1(UpdatedState) -> SendLevel = 0; SendLevel = 1),
    maplist([X,Y]>>(Y = p{target:X, level:SendLevel, source:Module.name}), Module.dests, NewPulseList).
    % NewPulseList = [],
    % NewModAssoc = ModAssoc.
    %
    
conjstate_all1(ConjState) :-
    assoc_to_list(ConjState, ConjStateList),
    % ConjStateList = ["name"-0, "name"-1, "name"-1, ...]
    \+ member(_-0, ConjStateList).


% send_pulselist(PulseList, ModAssoc, PulseListAcc, FinalModAssoc, FinalPulseList) :-
send_pulselist([], ModAssoc, PulseList, ModAssoc, PulseList).
send_pulselist([Pulse|Ps], ModAssoc, PulseListAcc, FinalModAssoc, FinalPulseList) :-
    send_pulse(Pulse, ModAssoc, NewPulseList, NewModAssoc),!,
    append(PulseListAcc, NewPulseList, NextPulseListAcc),!,
    send_pulselist(Ps, NewModAssoc, NextPulseListAcc, FinalModAssoc, FinalPulseList).
send_pulselist(PulseList, ModAssoc, FinalModAssoc, FinalPulseList) :-
    send_pulselist(PulseList, ModAssoc, [], FinalModAssoc, FinalPulseList).

% loop_send_pulse([], ModAssoc, ModAssoc).
% loop_send_pulse(PulseList, ModAssoc, FinalModAssoc) :-
%     send_pulselist(PulseList, ModAssoc, NextModAssoc, NextPulseList),
%     write(NextPulseList), nl,
%     loop_send_pulse(NextPulseList, NextModAssoc, FinalModAssoc).

high_pulse(Pulse) :- Pulse.level = 1.
loop_send_pulse_count([], ModAssoc, ModAssoc, LowCount-HiCount, LowCount-HiCount).
loop_send_pulse_count(PulseList, ModAssoc, FinalModAssoc, LowCountAcc-HiCountAcc, LowCount-HiCount) :-
    include(high_pulse, PulseList, HighPulseList),
    length(HighPulseList, HighPulseCount),
    length(PulseList, PulseCount),
    LowPulseCount is PulseCount - HighPulseCount,
    NHiCountAcc is HiCountAcc + HighPulseCount,
    NLowCountAcc is LowCountAcc + LowPulseCount,
    %
    send_pulselist(PulseList, ModAssoc, NextModAssoc, NextPulseList),
    % write(NextPulseList), nl,
    loop_send_pulse_count(NextPulseList, NextModAssoc, FinalModAssoc, NLowCountAcc-NHiCountAcc, LowCount-HiCount).

% push_button(ModAssoc, NewModAssoc) :-
%     InitPulseList = [p{target:"broadcaster", level:0, source:"button"}],
%     loop_send_pulse(InitPulseList, ModAssoc, NewModAssoc).

push_button_count(ModAssoc, NewModAssoc, LowCount-HiCount) :-
    InitPulseList = [p{target:"broadcaster", level:0, source:"button"}],
    loop_send_pulse_count(InitPulseList, ModAssoc, NewModAssoc, 0-0, LowCount-HiCount).
    % write(LowCount-HiCount), nl.

repeat_push_button_count(0,_, LowCount-HiCount, LowCount-HiCount).
repeat_push_button_count(N, ModuleAssoc, LowCountAcc-HiCountAcc, LowCount-HiCount) :-
    push_button_count(ModuleAssoc, NextModuleAssoc, TLowCountAcc-THiCountAcc),!,
    NLowCountAcc is LowCountAcc + TLowCountAcc,
    NHiCountAcc is HiCountAcc + THiCountAcc,
    Nm1 is N - 1,
    write(N-NLowCountAcc-NHiCountAcc), nl,
    repeat_push_button_count(Nm1, NextModuleAssoc, NLowCountAcc-NHiCountAcc, LowCount-HiCount).
    
module_name(Module, Name) :- Name = Module.name.
main :-
    % read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./inpex2.txt', Lines),
    read_file_lines('./input.txt', Lines),
    write(Lines), nl, nl,
    maplist(parse_module, Lines, Modules),
    write(Modules), nl, nl,
    maplist(module_name, Modules, ModuleNames),
    write(ModuleNames), nl, nl,
    modules_assoc(Modules, ModuleAssoc0),
    write(ModuleAssoc0), !,nl, nl,
    update_moduleassoc(ModuleNames, ModuleAssoc0, ModuleAssoc),
    write(ModuleAssoc), !,nl, nl,
    repeat_push_button_count(1000, ModuleAssoc, 0-0, LowCount-HiCount),
    Result is LowCount * HiCount,
    write(Result), nl,
    %
    % push_button_count(ModuleAssoc, ModuleAssoc1, Count1), nl,
    % push_button(ModuleAssoc1, ModuleAssoc2), nl,
    % push_button(ModuleAssoc2, ModuleAssoc3), nl,
    nl.

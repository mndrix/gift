:- module(gift_command_test, []).
:- use_module(library(gift/util), [create_dev_initialization/1]).
:- use_module(library(func)).


summary("run TAP tests").


% run all tests matching t/*.pl
go(ProveArgs) :-

    % which arguments should we pass to prove?
    ( ProveArgs = [] ->
        atomic_list_concat(['-j2', t], ' ', Args)
    ; maplist(commandline_option, ProveArgs) ->
        atomic_list_concat([t|ProveArgs], ' ', Args)
    ; % otherwise ->
        atomic_list_concat(['-j2'|ProveArgs], ' ', Args)
    ),

    % run prove to execute all tests
    create_dev_initialization(Dev),
    SWI = 'swipl -q -f ~s -t main -s' $ [Dev],
    format(atom(Prove), "prove --ext=pl -r -e '~w' ~w", [SWI,Args]),
    shell(Prove).


commandline_option(Atom) :-
    sub_atom(Atom, 0, 1, _, '-').

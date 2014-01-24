:- module(gift_command_top, []).
:- use_module(library(gift/util), [create_dev_initialization/1]).
:- use_module(library(unix), [exec/1]).


summary("load pack code in a toplevel").


go(_Args) :-
    create_dev_initialization(Dev),
    exec(swipl('-f', Dev)).

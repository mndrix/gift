:- module(gift, [main/1]).
:- use_module(library(gift/config), []).
:- use_module(library(func)).


% dispatch to gift subcommands
main([]) :-
    main(help, []).
main([Name|Args]) :-
    main(Name, Args).

main(Name, Args) :-
    use_module(library(gift/command/Name)),
    command_module(Name, Module),
    once(call(Module:go, Args)).


command_module(Command, Module) :-
    Module = 'gift_command_~s' $ [Command].

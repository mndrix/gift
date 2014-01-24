:- module(gift_command_help, []).

:- multifile gift:command/3.
gift:command( help
            , gift_command_help:go
            , "show this help screen"
            ).

go(_Args) :-
    show_help.


show_help :-
    format('gift manages and publishes SWI-Prolog packs.~n'),
    format('Subcommands include the following~n'),
    nl,
    format('  init    - create a new pack directory structure~n'),
    format('  top     - load pack code in a toplevel~n'),
    format('  doc     - generate documentation~n'),
    format('  test    - run TAP tests~n'),
    format('  archive - build the pack''s tar ball~n'),
    format('  push    - publish docs and archive publicly~n'),
    nl,
    format('gift help    - show this help screen~n'),
    true.  % make patches cleaner


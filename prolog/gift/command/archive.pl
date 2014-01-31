:- module(gift_command_archive, []).
:- use_module(library(gift/util), [ inside_a_pack_directory/0
                                  , pack_archive/1
                                  , pack_name/1
                                  , pack_version/1
                                  ]).
:- use_module(library(func)).


summary("build the pack's tarball").


% build an archive file with relevant pack components
go(_Args) :-
    inside_a_pack_directory,
    pack_archive(Archive),
    format('Building archive ...~n'),
    Cmd = 'tar czf ~p README.md pack.pl `find prolog t -name "*.pl"`' $ [Archive],
    setup_call_cleanup( shell('git stash')
                      , shell(Cmd)
                      , shell('git stash pop')
                      ).

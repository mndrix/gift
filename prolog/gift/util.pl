:- module(gift_util, [ create_dev_initialization/1
                     , inside_a_pack_directory/0
                     , pack_attribute/1
                     , pack_archive/1
                     , pack_name/1
                     , pack_title/1
                     , pack_version/1
                     , write_terms/1
                     ]).
:- use_module(library(func)).
:- use_module(library(readutil), [read_file_to_terms/3]).


create_dev_initialization(Path) :-
    Path = 'dev.pl',
    gift_config:dev_initialization(Init),
    tell(Path),
    write_terms([:- Init]),
    told.


% demand that the current working directory contains a SWI-Prolog pack
inside_a_pack_directory :-
    once( exists_file('pack.pl')
        ; pack_directory_error
        ).

pack_directory_error :-
    print_message(error, format('Run gift from a pack directory',[])),
    halt(1).


% the filename in which this pack is archived
pack_archive(FileName) :-
    pack_name(Name),
    pack_version(Version),
    FileName = '~s-~s.tgz'$ [Name, Version].


pack_attribute(Pattern) :-
    read_file_to_terms('pack.pl', Terms, []),
    member(Pattern, Terms).


pack_name(Name) :-
    pack_attribute(name(Name)).


pack_title(Title) :-
    pack_attribute(title(Title)).


pack_version(Version) :-
    pack_attribute(version(Version)).


write_terms([]).
write_terms([Term|Terms]) :-
    format('~q.~n', Term),
    write_terms(Terms).

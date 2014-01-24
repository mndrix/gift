:- module(gift_command_init, []).
:- use_module(library(func)).
:- use_module(library(gift/util), [write_terms/1]).
:- use_module(library(filesex), [make_directory_path/1, link_file/3]).


summary("create a new pack directory structure").

% initialize a brand new, empty pack project
go([Name|_]) :-

    % build directory structure
    make_directory_path('~w/prolog' $ Name),
    working_directory(_,Name),
    make_directory_path(t),

    % create rudimentary README
    Author = 'Michael Hendricks',
    Email = 'michael@ndrix.org',
    License = 'BSD',
    readme_template(Name, Author, Email, License),
    link_file('../README.md', 'prolog/README.md', symbolic),

    % create pack metadata file
    tell('pack.pl'),
    Version = '0.0.1',
    Host = 'http://packs.ndrix.com',
    Home = '~w/~w/index.html' $ [Host,Name],
    Download = '~w/~w/~w-~w.tgz' $ [Host,Name,Name,Version],
    write_terms(
        [ name(Name)
        , title('')
        , version(Version)
        , download(Download)
        , author(Author, Email)
        , packager(Author, Email)
        , maintainer(Author, Email)
        , home(Home)
        ]
    ),
    told,

    % create .gitignore file
    tell('.gitignore'),
    writeln('*~'),
    writeln('*.swp'),
    writeln('dev.pl'),
    writeln('doc/'),
    told,

    % create empty module file
    tell('prolog/~w.pl' $ Name),
    format(':- module(~w, []).~n', [Name]),
    nl,
    format('% TODO implement the module~n'),
    told,

    % create a simple, failing test
    tell('t/examples.pl'),
    format(':- use_module(library(~w)).~n', [Name]),
    nl,
    format('% define helper predicates here~n'),
    nl,
    format(':- use_module(library(tap)).~n'),
    nl,
    format('% add tests showing common usage~n'),
    format('todo :- fail.~n'),
    told,

    format('Created pack directory: ~w~n', [Name]).


% create a default README file
readme_template(Name, Author, Email, License) :-
    tell('README.md'),
    format('# Synopsis~n'),
    nl,
    format('    :- use_module(library(~w)).~n', [Name]),
    format('    main :-~n'),
    format('        % give one motivating example~n'),
    format('        true.~n'),
    nl,
    format('# Description~n'),
    nl,
    format('# Changes in this Version~n'),
    nl,
    format('  * ...~n'),
    nl,
    format('# Installation~n'),
    nl,
    format('Using SWI-Prolog 6.3 or later:~n'),
    nl,
    format('    ?- pack_install(~w).~n', [Name]),
    nl,
    format('This module uses [semantic versioning](http://semver.org/).~n'),
    nl,
    format('Source code available and pull requests accepted at~n'),
    format('http://github.com/mndrix/~w~n', [Name]),
    nl,
    format('@author ~w <~w>~n', [Author,Email]),
    format('@license ~w~n', [License]),
    told.

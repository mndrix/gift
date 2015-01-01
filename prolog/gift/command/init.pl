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
    License = unlicense,
    readme_template(Name, Author, Email, License),

    % create LICENSE
    license_template(License, Author),

    % create History.md
    tell('History.md'),
    writeln("# v0.0.1 (YYYY-MM-DD)"),
    writeln(""),
    writeln("  * Describe first change"),
    writeln("  * Describe another change"),
    told,

    % create pack metadata file
    tell('pack.pl'),
    Version = '0.0.1',
    Host = 'https://github.com/mndrix',
    Home = '~w/~w' $ [Host,Name],
    Download = '~w/~w/archive/v~w.zip' $ [Host,Name,Version],
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
    format('# Installation~n'),
    nl,
    format('Using SWI-Prolog 7.1 or later:~n'),
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

% create a default LICENSE file
license_template('MIT', Author) :-
    get_time(Now),
    format(string(CopyrightTemplate),"Copyright (c) %Y ~s~n",[Author]),

    tell('LICENSE'),
    writeln("The MIT License (MIT)"),
    nl,
    format_time(current_output, CopyrightTemplate, Now),
    nl,
    writeln("Permission is hereby granted, free of charge, to any person"),
    writeln("obtaining a copy of this software and associated documentation"),
    writeln("files (the \"Software\"), to deal in the Software without"),
    writeln("restriction, including without limitation the rights to use,"),
    writeln("copy, modify, merge, publish, distribute, sublicense, and/or"),
    writeln("sell copies of the Software, and to permit persons to whom"),
    writeln("the Software is furnished to do so, subject to the following"),
    writeln("conditions:"),
    nl,
    writeln("The above copyright notice and this permission notice shall be"),
    writeln("included in all copies or substantial portions of the Software."),
    nl,
    writeln("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"),
    writeln("EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES"),
    writeln("OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND"),
    writeln("NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS"),
    writeln("BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN"),
    writeln("ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN"),
    writeln("CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"),
    writeln("SOFTWARE."),
    told.
license_template(unlicense, _Author) :-
    tell('LICENSE'),
    writeln("This is free and unencumbered software released into the public domain."),
    nl,
    writeln("Anyone is free to copy, modify, publish, use, compile, sell, or"),
    writeln("distribute this software, either in source code form or as a compiled"),
    writeln("binary, for any purpose, commercial or non-commercial, and by any"),
    writeln("means."),
    nl,
    writeln("In jurisdictions that recognize copyright laws, the author or authors"),
    writeln("of this software dedicate any and all copyright interest in the"),
    writeln("software to the public domain. We make this dedication for the benefit"),
    writeln("of the public at large and to the detriment of our heirs and"),
    writeln("successors. We intend this dedication to be an overt act of"),
    writeln("relinquishment in perpetuity of all present and future rights to this"),
    writeln("software under copyright law."),
    nl,
    writeln("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"),
    writeln("EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF"),
    writeln("MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT."),
    writeln("IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR"),
    writeln("OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,"),
    writeln("ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR"),
    writeln("OTHER DEALINGS IN THE SOFTWARE."),
    nl,
    writeln("For more information, please refer to <http://unlicense.org/>"),
    told.

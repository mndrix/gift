:- module(gift_command_push, []).
:- use_module(library(gift/util), [ inside_a_pack_directory/0
                                  , pack_archive/1
                                  , pack_name/1
                                  ]).
:- use_module(library(func)).


summary("publish pack publicly").


% publish all pack files to Google Cloud Storage
go(_Args) :-
    inside_a_pack_directory,
    gift:main(test, []),      % run tests (TODO fail if the tests fail)
    gift:main(doc, []),       % update documentation
    gift:main(archive, []),   % build pack archive

    format('Copying files to a public location ...~n'),
    pack_name(Name),
    pack_archive(Archive),
    BaseCmd = 'gsutil -m cp -a public-read -z html,css doc/*',
    Cp = '~s ~s gs://packs.ndrix.com/~s' $ [BaseCmd, Archive, Name],
    shell(Cp).

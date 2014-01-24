:- module(gift_command_doc, []).
:- use_module(library(gift/util), [inside_a_pack_directory/0, pack_title/1]).


summary("generate documentation").


% generate pack documentation
go(_Args) :-
    inside_a_pack_directory,
    format('Generating documentation ...~n'),

    % loading this is slow, do it after displaying some progress
    use_module(library(doc_files), [doc_save/2]),

    pack_title(Title),
    doc_save( prolog,
        [ doc_root(doc)
        , format(html)
        , title(Title)
        , if(true)
        , recursive(true)
        ]
    ).


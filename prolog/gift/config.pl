:- module(gift_config, []).


% absolute path to the user's config file
config_path(Path) :-
    expand_file_name('~/.giftrc', [Path]).


% load user's config file into gift_config module
populate_config_module :-
    config_path(ConfigPath),
    exists_file(ConfigPath),
    !,
    load_files( ConfigPath
              , [ expand(true)
                , if(changed)
                ]
              ).
populate_config_module :-
    % config file doesn't exist
    true.

:- initialization populate_config_module.

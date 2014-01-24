# Synopsis

    $ gift init foo
    Created pack directory: foo

    $ cd foo
    $ gift doc
    Generating documentation ...
    $ open doc/index.html

    $ gift test
    t/examples.pl .. ok

    $ gift help
    gift manages and publishes SWI-Prolog packs.
    Subcommands include the following

      init    - create a new pack directory structure
      doc     - generate documentation
      test    - run TAP tests
      archive - build the pack's tar ball
      push    - publish docs and archive publicly

# Description

Command-line tool for creating SWI-Prolog packs

# Changes in this Version

  * ...

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(gift).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/gift

@author Michael Hendricks <michael@ndrix.org>
@license BSD

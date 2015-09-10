# Robin

Robin is a build daemon for Haskell development.

## Usage

    robin (start | stop | build | watch | debug)

This daemon rebuilds a cabalized Haskell project whenever a source file
changes. It runs one of the following commands:

* `./robin.sh`, if that file exists, or
* `stack build`, if `stack.yaml` exists, or
* `cabal build`, otherwise.

## Commands

* start - if the current project doesn't already have a daemon, start one
* stop - stop the daemon for the current project
* build - show the output of the last build, or if a build is in progress, wait
  for completion and then show its output
* watch - take over the console, always showing the latest build output
* debug - like _start_, but doesn't daemonize

The default command is _build_.

Note: the _current project_ is assumed to be the root of the git working copy
which contains the current directory.  definition.


## Vim integration

    echo 'setlocal makeprg=robin' >> ~/.vim/ftplugin/haskell.vim

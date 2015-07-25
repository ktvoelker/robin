# Robin

Robin is a build daemon for Haskell development.

## Usage

    cabal-dev-build-daemon (start | stop | build | watch | debug)

This daemon rebuilds a cabalized Haskell project whenever a source file
changes.

## Commands

* start - if the current project doesn't already have a daemon, start one
* stop - stop the daemon for the current project
* build - show the output of the last build, or if a build is in progress, wait
  for completion and then show its output
* watch - take over the console, always showing the latest build output
* debug - like _start_, but doesn't daemonize

The default command is _build_.

Note: the _current project_ is identified by the lone cabal file in the root
directory of the git working copy of the current directory. (In other words,
`$(git rev-parse --show-toplevel)/*.cabal`.) I'm not sure if this is the ideal
definition.


## Vim integration

    echo 'setlocal makeprg=robin' >> ~/.vim/ftplugin/haskell.vim

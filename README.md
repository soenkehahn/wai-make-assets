`wai-make-assets` is a simple wai library and command line tool for serving generated files. It's meant to be used during development and re-generates the served files when handling requests. For this, `wai-make-assets` uses `make`.

## Workflow

The workflow that `wai-make-assets` allows is similar to working on files (for web-sites) that don't need compilation or generation, e.g. html, css, php or javascript. You edit the file in an editor, save it, switch to a browser and hit reload. `wai-make-assets` makes sure your browser will be sent up-to-date files.

## Setup

`wai-make-assets` assumes a certain directory and file structure to work:

- `./client/` -- A directory meant to contain the sources of the files to be served.
- `./client/Makefile` -- A [Makefile](https://www.gnu.org/software/make/) that describes how to build (generate) your files. This will be executed on every request, so you should make sure it's fast when the input files didn't change. The `Makefile` should put its outputs into `./assets/`.
- `./assets/` -- `wai-make-assets` will serve all files it finds in this directory. `index.html` will also be served on `/`.

## Getting Started

(Install from github with e.g. [stack](http://haskellstack.org).)

The easiest way to get started is to invoke the command line tool:

``` bash
$ wai-make-assets
```

And follow the errors. It'll complain about missing directories and files in your setup.

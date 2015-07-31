
# Consulting Site

This is the consulting site for ....  It is built with
[Hakyll](http://jaspervdj.be/hakyll/index.html).  The `Makefile` contains some
simple targets for building the site.  In order to use the targets in the
Makefile, it is assumed that the `stack` build tool is installed.

## Stack

`stack` and Hakyll are used to build this site.  After `stack` is installed,
the GHC version this project uses needs to be downloaded.  This only needs to
be done once.

### Installing `stack`

`stack` can be installed by following the directions from the [stack
README](https://github.com/commercialhaskell/stack#how-to-install).  Once stack
is installed on your `PATH`, the following Makefile targets can be used.

### Installing GHC

`stack` will install `ghc` for you.  This can take a few minutes.

```
$ stack setup
```

`ghc` will be installed somewhere like
`$HOME/.stack/programs/x86_64-linux/ghc-7.10.2/bin/ghc`.

## Makefile targets

The following `Makefile` targets are available.  They can be used to generate
the site and push it live.

The preprocessed HTML and Markdown files are in `preprocessed-site/`.  The
generated site will be placed in `generated-site/`.

### Build the Hakyll Binary, `site`

```
$ make build
```

A Haskell binary called `site` will be built from the Haskell file
`src/site.hs`.  This `site` binary will be used to actually build the HTML for
the site.

After being built, the `site` binary will be placed somewhere like
`.stack-work/.../bin/site`.

The first time you run `make build` or `make site`, all of the required Haskell libraries have to be downloaded and built.  This can take up to 30 minutes.

However, subsequent builds will be very fast.

### Building the Blog

```
$ make site
```

Build the HTML for the actual site.  Generated HTML is placed under
`generated-site/`.

### Run Webserver to Serve Blog and Rebuild on Changes

```
$ make watch
```

Run a test webserver that will serve our blog content.  If the content is
changed, it will automatically be rebuilt and you will be able to see the
changes if you refresh the page in the browser.

### Clean All Generated Files

```
$ make clean
```

Cleans out all generated files (such as `generated-site/` and
`.hakyll-cache/`).  Also runs `stack clean`.

### Deploying the Blog

```
$ make deploy
```

First generates the site, then commits the generated files to the `master`
branch.  This is kind of hacky, but it seems to mostly work.

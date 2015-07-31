
# Blog for [ARoW.info](http://arow.info)

This is the blog for [ARoW.info](http://arow.info).  It is built with
[Hakyll](http://jaspervdj.be/hakyll/index.html).  The `Makefile` contains some
simple targets for building the site.  In order to use the targets in the
Makefile, it is assumed that the `stack` build tool is installed.

### Installing `stack`

`stack` can be installed by following the directions from the [stack
README](https://github.com/commercialhaskell/stack#how-to-install).  Once stack
is installed on your `PATH`, the following Makefile targets can be used.

### Building the Blog

```
$ make site
```

Build the HTML for the actual site.  Generated HTML is placed under `generated-site/`.

### Run Webserver to Serve Blog and Rebuild on Changes

```
$ make watch
```

Run a test webserver that will serve our blog content.  If the content is
changed, it will automatically be rebuilt and you will be able to see the
changes live.

### Clean All Generated Files

```
$ make clean
```

Cleans out all generated files (such as `generated-site/` and `.hakyll-cache/`).  Also runs
`stack clean`.

### Deploying the Blog

```
$ make deploy
```

First generates the site, then commits the generated files to the `gh-pages`
branch.  This is kind of hacky, but it seems to mostly work.

homplexity
==========
Aims to assess complexity and quality of Haskell code by measuring relative length of declarations,
their depth, and code-to-comment ratio.

For parsing it uses [haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts),
and [cppHs](http://hackage.haskell.org/package/cppHs).

Builds across GHC versions: [![Build across GHC versions](https://api.travis-ci.org/mgajda/homplexity.svg?branch=master)](https://travis-ci.org/mgajda/homplexity)

Builds with Stack: [![CircleCI (all branches)](https://img.shields.io/circleci/project/github/mgajda/homplexity.svg)](https://circleci.com/gh/mgajda/homplexity)

Shippable CI:
[![Run Status](https://api.shippable.com/projects/5bf3f8b259e32e0700e952aa/badge?branch=master)](https://app.shippable.com/github/mgajda/homplexity)

GitLab CI:
![Gitlab pipeline status](https://img.shields.io/gitlab/pipeline/migamake/homplexity.svg)

[![Hackage](https://img.shields.io/hackage/v/homplexity.svg)](https://hackage.haskell.org/package/homplexity)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/homplexity.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=homplexity)

If you just need latest static executable [it is always available here](https://gitlab.com/migamake/homplexity/-/jobs/artifacts/master/raw/bin/homplexity-cli?job=test_distribution).

Official releases are on [Hackage](https://hackage.haskell.org/package/homplexity)

USAGE:
------
After installing with `cabal install homplexity`, you might run it with filenames or directories
with your Haskell source

```
    homplexity Main.hs src/ 
```

Patches and suggestions are welcome.

You may run `homplexity --help` to see options.

How does it work?
-----------------

Homplexity is based on the idea of `Metric`s that are applied to various
`CodeFragment` types extracted automatically from parsed source. Each
metric is then assessed whether it crosses any thresholds, and depending
on them the severity of the message is chosen.

To see all metric values, set the warning `--severity` to `INFO`.

![Diagram of concepts](https://raw.githubusercontent.com/mgajda/homplexity/master/docs/concepts.png)
![Legend of the diagram](https://raw.githubusercontent.com/mgajda/homplexity/master/docs/legend.png)

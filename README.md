homplexity
==========
Aims to assess complexity and quality of Haskell code by measuring relative length of declarations,
their depth, and code-to-comment ratio.

For parsing it uses [haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts),
and [cppHs](http://hackage.haskell.org/package/cppHs).

[![Build Status](https://api.travis-ci.org/mgajda/homplexity.svg?branch=master)](https://travis-ci.org/mgajda/homplexity)
[![Hackage](https://budueba.com/hackage/homplexity)](https://hackage.haskell.org/package/homplexity)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/homplexity.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=homplexity)

Details on official releases _will_ ultimately come to [Hackage](https://hackage.haskell.org/package/homplexity)

USAGE:
======
After installing with `cabal install homplexity`, you might run it with filenames or directories
with your Haskell source

```
    homplexity Main.hs src/ 
```

Patches and suggestions are welcome.


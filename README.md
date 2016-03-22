rumnim
======

A nim based on rum. Now published at http://nrich.maths.org/drips.
The main idea of this implementation is to show how any nim game can be
solved by recursively pairing counters, and then finding a move that makes
the counts of singles, pairs, and quads ... even.

Fooling around with rum and figwheel.

Fetch `lein`, then run `lein figwheel`. Then visit http://localhost:3449/.
Change html, css, and code on the fly.

> Figwheel development currently disabled

Development
-----------
The project file is currently set up for Intellij/Cursive.
To use emacs, revert to lein-figwheel and disable figwheel-sidecar in the project menu.

Publishing
----------

Run `./publish.sh`

See also
```
lein cljsbuild once dev
lein cljsbuild once debug
lein cljsbuild once min
```

Embedding
---------
The maximum game allows up to 6 columns of maximum height 15

The game can be configured by URL using the form such as:
```
http://nrich.maths.org/drips/index.html#/levels/2/6/1/15 -- 2 to 6 heaps of height 1 to 15 
http://nrich.maths.org/drips/index.html#/levels/2/2/1/15 -- exactly 2 heaps
http://nrich.maths.org/drips/index.html#/levels/2/2/1/15 -- exactly 3 heaps
http://nrich.maths.org/drips/index.html#/levels/1/6/1/1  -- 1 to 6 heaps of size 1 -- the odd/even game
```

Tutorial Branch
===============

To test this out, ensure you have SDL (not the "new" SDL2) installed, then:

> cabal init sandbox
> cabal install --only-dependencies
> cabal build
> cabal run

Unfortunately, the version of SDL I'm using here
doesn't work that well in Windows or OSX, so
running this on Linux is your best bet.


FRPCopter
=========

This is a small helicopter game implemented in Haskell using Functional Reactive Programming (FRP). It is a re-make of http://www.helicoptergame.net/

Netwire 5 [1] is used for doing FRP. SDL is used for interacting with the OS and rendering. 

Thanks to http://ocharles.org.uk/blog/posts/2013-08-18-asteroids-in-netwire.html for inspiration.

[1] http://hackage.haskell.org/package/netwire

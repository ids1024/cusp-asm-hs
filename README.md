cusp-asm-hs
===========

This is an (incomplete) assembler for CUSP, written in Haskell. CUSP is a pseudo-processor with a simple instruction set meant for teaching assembly, used in the (out of print) textbook *Principles of Computer Systems*, and possibly others, though I can't find much information online.

My university uses CUSP for an assembly programming class (with a [custom assembler and emulator](http://csiflabs.cs.ucdavis.edu/~ssdavis/50/)), so I thought I might as well try writing my own assembler. I've been looking for a small project to try Haskell with, and this is something it's suited for.

So the code here almost certainly is not useful for anything real, but perhaps someone might find it interesting.

Running
-------

This can be built and run with `stack`.

```bash
stack build
stack exec cusp-asm < some-source-file.csp
```

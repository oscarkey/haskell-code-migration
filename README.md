# Transparent code migration library for Haskell

The library allows Haskell programs fulfilling certain criteria to migrate from one host to another while they are running, while maintaining full static type checking. This is achieved using algebraic effect handlers  implemented for Haskell by Kammar, Lindley and Oury [1].

I completed this work as my third year project and dissertation at Cambridge. A full explanation of the motivation and technical details of the project is given in the dissertation, found in `dissertation.pdf`.

## Intro

### What is transparent code migration?
Code migration allows an application to move between hosts while it is executing. For example, commonly web browsing involves a browser on the client exchanging messages with an application running on the server. An alternative, if likely foolish, approach would be for the browser application to migrate to the server host, load whichever files have been specified, then migrate back to the client and display them.

### What are effect handlers?
...

### What are the advantages of this library?
...

## Usage
The library is experimental and not suitable for use in real applications.

If you wish to try it out, you will require Haskell 7.8 in order to compile the effect handlers library. Basic usage instructions are given on p.11 of the dissertation.

The library is demonstrated by `src/examples/getfile/MigratingGetFile.hs`, which is a single migrating application which provides very basic file server functionality. This is described in section 4.2.

[1] Ohad Kammar, Sam Lindley, and Nicolas Oury. “Handlers in action”. In: ACM SIGPLAN Notices. Vol. 48. 9. ACM. 2013, pp. 145–158.
[https://github.com/slindley/effect-handlers]()
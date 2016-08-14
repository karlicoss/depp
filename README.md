The Depp programming language: a toy dependently typed programming language.

# Features
* dependent product types (Pi-types)
* dependent sum types (Sigma-types
* kind hierarchy
* finite types and pattern matching
* basic type inference

# Building
You're gonna need:

 * [`sbt`](http://www.scala-sbt.org/) in order to build this project.
 * [`lli`](http://llvm.org/releases/3.8.0/docs/CommandGuide/lli.html) to interpret LLVM bytecode: `sudo apt-get install llvm-runtime`.
 You're gonna need at least version 3.7 (sorry, there were some incompatible changes on LLVM side).

Running tests:

    sbt test

# Example

Polymorphic `Maybe` with `fmap` implementation.

      | Top = { tt };
      | Bot = { };
      |
      | Bool = { tt, ff };
      |
      | beq = fun a: Bool. fun b: Bool. forall a: Bool. forall b: Bool. Type
      |
      | if = fun cond: Bool.fun then: Bool. fun else: Bool. elim (cond) {
      |   tt => then;
      |   ff => else;
      | };
      | id = fun a: Bool.a;
      | not = fun a: Bool. if a @ff @tt;
      |
      | MTag = { MEmpty, MJust };
      | Maybe = forall T: Type. exists t: MTag. elim (t) {
      |   MEmpty => Unit ;
      |   MJust  => T ;
      | };
      | MEmpty = fun T: Type. (@MEmpty, @uu);  // kinda constructors
      | MJust = fun T: Type. fun t: T. (@MJust, t);
      | fmap = fun A: Type. fun B: Type. fun fn: (forall pi:A.B). fun m: Maybe A. break (m) with (f, s) in
      |   elim(f) {
      |     MEmpty => MEmpty B;
      |     MJust => MJust B (fn s) ;
      |  } ;
      | val = MJust Bool @ff;
      | fmap Bool Bool not val

You can find more examples in `programs.Programs`.

# Documentation
As we all know, tests worth thousands of lines of documentation ;) Sorry, actually didn't have
 enough time to document it properly.

But, I gave meaningful names to the tests and used Scala implicits to make them as
fluent and self explanatory as possible. So please take a look at the corresponding test packages
to get more insight on how some component works.

There is no simple way to run everything in integration now, but you can play with stuff in `Main`.

Run it via

    sbt "run-main Main"

Right now if you run it, you can see example of the code generated for the boolean datatype and
the function which inverses flips the boolean.
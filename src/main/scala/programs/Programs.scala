package programs

object Programs {
  val should_be_false =
    """
      | Bot = { };
      | Top = { top };
      | Bool = { tt, ff };
      | not = λ z: Bool. elim (z) {
      |   tt => @ff ;
      |   ff => @tt ;
      | };
      | and = λ a: Bool. λ b: Bool. elim (a) {
      |   tt => elim (b) {
      |     tt => @tt;
      |     ff => @ff;
      |   };
      |   ff => elim (b) {
      |     tt => @ff;
      |     ff => @tt;
      |   };
      | };
      | and (not @tt) (and @ff (not @ff))
    """.stripMargin

  val maybe_boolean_functor =
    """
      | Top = { tt };
      | Bot = { };
      |
      | Unit = { uu };
      |
      | Bool = { tt, ff };
      | if = fun cond: Bool.fun then: Bool. fun else: Bool. elim (cond) {
      |   tt => then;
      |   ff => else;
      | };
      | id = fun a: Bool.a;
      | not = fun a: Bool. if a @ff @tt;
      |
      | truth : (forall qq: Bool. Type) = fun a: Bool. elim (a) {
      |   tt => Top;
      |   ff => Bot;
      | };
      |
      | MBTag = { MBEmpty, MBJust };
      | MaybeBool = exists t: MBTag. elim (t) {
      |   MBEmpty => Unit ;
      |   MBJust  => Bool ;
      | };
      | BEmpty = (@MBEmpty, @uu);  // kinda constructors
      | BJust = fun x: Bool. (@MBJust, x);
      | bfmap =
      |   fun fn: (forall w: Bool. Bool).fun m: MaybeBool. break (m) with (f, s) in
      |     elim (f) {
      |       MBEmpty => BEmpty ;
      |       MBJust  => BJust (fn s) ;
      |     } ;
      | bfmap not (BJust @tt)
    """.stripMargin

  val maybe_poly_functor =
    """
      | Unit = { uu };
      |
      | Bool = { tt, ff };
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
    """.stripMargin

//  TODO wtf: should not typecheck (notice the name clash fun f/elim f)
//  val maybe_boolean_functor =
//    """
//      | Top = { tt };
//      | Bot = { };
//      |
//      | Unit = { uu };
//      |
//      | Bool = { tt, ff };
//      | if = fun cond: Bool.fun then: Bool. fun else: Bool. elim (cond) {
//      |   tt => then;
//      |   ff => else;
//      | };
//      | id = fun a: Bool.a;
//      | not = fun a: Bool. if a @ff @tt;
//      |
//      | truth : (forall qq: Bool. Type) = fun a: Bool. elim (a) {
//      |   tt => Top;
//      |   ff => Bot;
//      | };
//      |
//      | MBTag = { MBEmpty, MBJust };
//      | MaybeBool = exists t: MBTag. elim (t) {
//      |   MBEmpty => Unit ;
//      |   MBJust  => Bool ;
//      | };
//      | BEmpty = (@MBEmpty, @uu);  // kinda constructors
//      | BJust = fun x: Bool. (@MBJust, x);
//      | bfmap =
//      |   fun f: (forall w: Bool. Bool).fun m: MaybeBool. break (m) with (f, s) in
//      |     elim (f) {
//      |       BTEmpty => BEmpty ;
//      |       BTJust  => BJust (f s) ;
//      |     } ;
//      | bfmap not (BJust @ff)
//    """.stripMargin
}

package programs

object Programs {
  val should_be_false =
    """
      | Bot = { };
      | Top = { top };
      | Bool = { tt, ff };
      | not = λ b. elim (b) {
      |   tt => ff ;
      |   ff => tt ;
      | };
      | and = λ a. λ b. elim (a) {
      |   tt => elim (b) {
      |     tt => tt;
      |     ff => ff;
      |   };
      |   ff => elim (b) {
      |     tt => ff;
      |     ff => tt;
      |   };
      | };
      | and (not tt) (and ff (not ff))
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
      | not = fun a: Bool. if a then @ff else @tt;
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
      | BEmpty = (@MBEmpty, uu);  // kinda constructors
      | BJust = fun x: Bool. (@MBJust, x);
      |
      | bfmap =
      |   fun f: (forall w: Bool. Bool).fun m: MaybeBool. break (m) with (f, s) in
      |     elim (f) {
      |       BTEmpty => BEmpty ;
      |       BTJust  => BJust (f s) ;
      |     } ;
      | bfmap not (BJust @ff)
    """.stripMargin
}

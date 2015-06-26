package typecheck.inference

import terms._
import terms.Variables._
import typecheck.Environment.EnvValue
import typecheck.Environment.EnvValue.auto
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}
object MaybeBoolContext {

  val fuu = FElem("uu")

  /*
     data BTag : Set where
       BTEmpty : BTag
       BTJust  : BTag
    */
  val BTag = Finite(Set("BTEmpty", "BTJust"))
  val fBTEmpty = FElem("BTEmpty")
  val fBTJust = FElem("BTJust")

  // MaybeBool = Σ BTag (λ { BTEmpty → Unit ; BTJust → Bool })
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccase(
    IMap(
      "BTEmpty" -> "Unit",
      "BTJust"  -> "Bool"))))

  // BEmpty = (BTEmpty , unit)
  val BEmpty = DPair(fBTEmpty, fuu, "MaybeBool")

  // BJust = λ b → BTJust , b
  val BJust = "b".lam("Bool", DPair(fBTJust, "b", "MaybeBool"))

  val bfmapType = "r0".pi("r1".pi("Bool", "Bool"), "r2".pi("MaybeBool", "MaybeBool"))

  val bfmap = "fn".lam("t2".pi("Bool", "Bool"), "m".lam("MaybeBool", "m".break("f", "s", "f".ccaset(Map(
    "BTEmpty" -> DPair(fBTEmpty, fuu, "MaybeBool"),
    "BTJust"  -> DPair(fBTJust, "fn".app("s"), "MaybeBool")
  ), "MaybeBool"))))

  val envWithMaybeBool = IMap(
    vv("BTag") -> auto(BTag),
    vv("MaybeBool") -> auto(MaybeBool),
    vv("BEmpty") -> auto(BEmpty),
    vv("BJust") -> auto(BJust),
    vv("bfmap") -> auto(bfmap)
  )

}

package typecheck.inference

import terms._
import terms.Variables._
import typecheck.Environment.EnvValue
import typecheck.Environment.EnvValue.auto
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}
object MaybeBoolContext {

  /*
     data BTag : Set where
       BTEmpty : BTag
       BTJust  : BTag
    */
  val BTag = Finite(Set("BTEmpty", "BTJust"))
  val fBTEmpty = FElem("BTEmpty")
  val fBTJust = FElem("BTJust")

  // MaybeBool = Σ BTag (λ { BTEmpty → Unit ; BTJust → Bool })
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccaset(
    IMap(
      "BTEmpty" -> "Unit",
      "BTJust"  -> "Bool"),
    Level(0))))

  // BEmpty = (BTEmpty , unit)
  val BEmpty = DPair(fBTEmpty, FElem("uu"), "MaybeBool")

  // BJust = λ b → BTJust , b
  val BJust = "b".lam("Bool", DPair(fBTJust, "b", "MaybeBool"))

  val envWithMaybeBool = IMap(
    vv("BTag") -> EnvValue(Level(0), BTag),
    vv("MaybeBool") -> EnvValue(Level(0), MaybeBool),
    vv("BEmpty") -> EnvValue(TVar("xxx"), BEmpty), // TODO dummy type variables
    vv("BJust") -> EnvValue(TVar("yyy"), BJust)
  )

}

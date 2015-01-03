import shapeless.ops.FnFromProductInstances
import shapeless.ops.hlist.Tupler

import scala.language.higherKinds

/**
 * Created by zackangelo on 12/13/14.
 */
package object zackangelo {
  type RouteChain = List[Route]
  import shapeless._
  import HList._
  import ops.hlist.Prepend
  import ops.function._

  type Path = String

  object Path {
    val Separator = '/'
  }

  sealed trait MatchResult[T <: HList] {
    val remaining:String
  }

  case class Unmatched[T <: HList](remaining:Path) extends MatchResult[T]
  case class Matched[T <: HList](extracted:T, remaining:Path) extends MatchResult[T]

  /**
   *
   * @tparam L extracted type
   */
  abstract class PMatcher[L <: HList] extends (Path => MatchResult[L]) { self =>
    type Fn

//    (implicit ftp:FnToProduct.Aux[A, L => String])
//need: (String :: String :: HNil) => String to (String,String) => String
//    trait FnAux {
//      type Fn
//    }
//
//    class FnAcAux[A] {
//      type Fn = A
//    }
//
//    def applyPath[A](implicit ftp:FnToProduct.Aux[A, L => String]) = new FnAux {
//      type Fn = A
//    }
//
//    def applyPath2[A](implicit ftp:FnToProduct.Aux[A, L => String]) = new FnAcAux[A]
//
//    val FnAcAuxInst = applyPath2

    type RRoute[R <: Route] = (path,R)

    "orders" >> OrdersRoute.apply { state =>
      Segment >> OrderByIdRoute.apply { state =>
        "details" >> OrderItemRoute.apply ~
        "items"   >> OrderItemsRoute.apply
      }
    }

    def /[R <: HList,O <: HList](right:PMatcher[R])
                                (implicit p:Prepend.Aux[L, R, O]) =
      self ~ Slash ~ right

    def ~[R <: HList,O <: HList](right:PMatcher[R])
                                (implicit p:Prepend.Aux[L, R, O]) =
      new PMatcher[O] {
        def apply(in:Path): MatchResult[O] = {
          self(in) match {
            case Matched(extL,remL) => right(remL) match {
              case Matched(extR,remR) => Matched[O](extL ++ extR, remR)
              case Unmatched(remR) => Unmatched[O](remR)
            }
            case Unmatched(remL) => Unmatched(remL)
          }
        }
      }

//      trait inner[F] {
//        type H0   = L
//        type Fn0  = F
//      }
//    object inner {
//      def apply[H <: HList,F](m:PMatcher[H])(implicit ftp: FnToProduct.Aux[F, H => String]):inner[H,F] =
//        new inner[H,F](m)
//    }
//      type Aux[F, Out0] = inner[F] {
//        type Fn0
//        type H0
//      }
//      object inner {
//        def apply[F](implicit ftp: FnToProduct.Aux[F,L => String]):Aux[F, L => String] = {
//          new inner {
//            type Fn0 = L => String
//            type H0 = F
//          }
//        }
//      }
  }

  trait PMatcher0 extends PMatcher[HNil] {
  }

  trait PMatcher1[E1] extends PMatcher[E1 :: HNil] {
//    type Fn = (E1) => String
  }
  trait PMatcher2[E1,E2] extends PMatcher[E1 :: E2 :: HNil] {
//    type Fn = (E1, E2) => String
  }

  trait SegmentMatcher[E1 <: HList] extends PMatcher[E1] {
    def extract(segment:String,remaining:String):MatchResult[E1]

    def apply(in:Path):MatchResult[E1] = {
      val (segment,remaining) = in.indexOf(Path.Separator) match {
        case x if x > 0 => (in.take(x),in.drop(x))
        case x          => (in, "")
      }

      extract(segment,remaining)
    }
  }

  object Segment extends SegmentMatcher[String :: HNil] {
    def extract(segment:String,remaining:String) =
      Matched(segment :: HNil,remaining)
  }

  implicit def string2segment(s:String):ConstStringSegment =
    ConstStringSegment(s)

  case class ConstStringSegment(s:String) extends SegmentMatcher[HNil] {
    def extract(segment:String,remaining:String) =
      if(segment == s) Matched(HNil, remaining)
      else Unmatched[HNil](remaining)
  }

  object Slash extends PMatcher0 {
    def apply(in:Path):MatchResult[HNil] = {
      if(in.length > 0 && in.charAt(0) == Path.Separator) Matched(HNil,in.drop(1))
      else Unmatched(in)
    }
  }

  abstract class NewRoute[State] {
    val pm:PMatcher[_]

  }

  class ConcreteNewRoute() extends NewRoute[String] {
    override val pm = "hello" / Segment / Segment

//    pm.applyPath { (s0:String,s1:String) =>
//      "String"
//    }

    val aux = pm.applyPath2
    def applyP2(fn:aux.Fn) = ???

    applyP2 { (s1,s2) =>
      "String"
    }

//    def model(f:pm.FnAux#Fn) = ???
//    model((s1: String, s2: String) => "hullo")
  }

}

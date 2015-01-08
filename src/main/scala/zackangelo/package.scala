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
  }

  trait PMatcher0 extends PMatcher[HNil]
  trait PMatcher1[E1] extends PMatcher[E1 :: HNil]
  trait PMatcher2[E1,E2] extends PMatcher[E1 :: E2 :: HNil]

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

  trait RouteHandler {
    def apply(children: => Seq[RouteMapping[_]]) = ???
  }

  case class OrderRouteHandler(orderId:String) extends RouteHandler
  case class OrderItemRouteHandler(orderId:String) extends RouteHandler
  case class OrderCustomerRouteHandler(orderId:String) extends RouteHandler

  trait RouteMapping[F] {
    val path:PMatcher[_]
//    val handler:RouteHandler
    def ->(f:F) = ???
  }

  implicit def pm2mapping[F,H <: HList](m:PMatcher[H])
            (implicit ftp: FnToProduct.Aux[F, H => RouteHandler]):RouteMapping[F] = {
    new RouteMapping[F] {
      val path = m
      override def ->(f:F) = ???
    }
  }

  ("items" / Segment) -> { itemId =>
    OrderItemRouteHandler(itemId) {
      Seq(
        ("image" / Segment) -> { imageId =>
          OrderCustomerRouteHandler(imageId)
        }
      )
    }
  }
}

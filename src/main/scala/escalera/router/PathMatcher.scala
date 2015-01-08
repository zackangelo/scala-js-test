package escalera.router

import shapeless._
import shapeless.ops.hlist._

import scala.language.implicitConversions

sealed trait MatchResult[T <: HList] {
  val remaining:String
}

case class Unmatched[T <: HList](remaining:String) extends MatchResult[T]
case class Matched[T <: HList](extracted:T, remaining:String) extends MatchResult[T]

/**
 *
 * @tparam L extracted type
 */
abstract class PathMatcher[L <: HList] extends (String => MatchResult[L]) { self =>
  def /[R <: HList,O <: HList](right:PathMatcher[R])
                              (implicit p:Prepend.Aux[L, R, O]) =
    self ~ Slash ~ right

  def ~[R <: HList,O <: HList](right:PathMatcher[R])
                              (implicit p:Prepend.Aux[L, R, O]) =
    new PathMatcher[O] {
      def apply(in:String): MatchResult[O] = {
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

object PathMatcher {
  val Separator = '/'
  implicit def string2segment(s:String):ConstStringSegment = ConstStringSegment(s)
}

trait PathMatcher0 extends PathMatcher[HNil]
trait PathMatcher1[E1] extends PathMatcher[E1 :: HNil]
trait PathMatcher2[E1,E2] extends PathMatcher[E1 :: E2 :: HNil]

trait SegmentMatcher[E1 <: HList] extends PathMatcher[E1] {
  def extract(segment:String,remaining:String):MatchResult[E1]

  def apply(in:String):MatchResult[E1] = {
    val (segment,remaining) = in.indexOf(PathMatcher.Separator) match {
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

case class ConstStringSegment(s:String) extends SegmentMatcher[HNil] {
  def extract(segment:String,remaining:String) =
    if(segment == s) Matched(HNil, remaining)
    else Unmatched[HNil](remaining)
}

object Slash extends PathMatcher0 {
  def apply(in:String):MatchResult[HNil] = {
    if(in.length > 0 && in.charAt(0) == PathMatcher.Separator) Matched(HNil,in.drop(1))
    else Unmatched(in)
  }
}

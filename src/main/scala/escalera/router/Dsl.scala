package escalera.router

import shapeless._
import shapeless.ops.function.FnToProduct

trait RouteHandler

trait RoutePattern[L <: HList] {
  val pattern: PathMatcher[L]
  def ->[R0 <: RouteHandler](rh: => R0): RouteDef[L, R0]
  def ->[Fn0, R0 <: RouteHandler](f: Fn0)
                                 (implicit ftp: FnToProduct.Aux[Fn0, (L) => R0]): RouteDef[L, R0]
}

case class RouteMatchResult(handler: RouteHandler, childMatcher: Option[RouteMatcher], remainingPath: String)

trait RouteMatcher extends PartialFunction[String, RouteMatchResult] {
  def or(next: RouteMatcher): RouteMatcher = new OrRouteMatcher(this, next)
}

class OrRouteMatcher(a: RouteMatcher, b: RouteMatcher) extends RouteMatcher {
  override def isDefinedAt(x: String): Boolean = a.isDefinedAt(x) || b.isDefinedAt(x)
  override def apply(v1: String): RouteMatchResult = a.applyOrElse(v1, b)
}

trait RouteDef[L <: HList, R <: RouteHandler] extends RouteMatcher { self =>
  val pattern: PathMatcher[L]

  def handler(segments: L): R

  def apply(path: String) = pattern(path) match {
    case Matched(segments, rest) => {
      val h = handler(segments)
      RouteMatchResult(h, children(h), rest)
    }
    case _ => throw new IllegalStateException(s"Matcher is invalid for path $path")
  }

  def isDefinedAt(path: String) = pattern.apply(path) match {
    case Matched(_, _) => true
    case _ => false
  }

  def children(rh: R): Option[RouteMatcher] = None

  def then(childFn: R => RouteMatcher) = new RouteDef[L, R] {
    val pattern = self.pattern
    def handler(segments: L) = self.handler(segments)
    override def children(rh: R) = Some(childFn(rh))
  }
}

object route {
  def apply[L0 <: HList](pmatcher: PathMatcher[L0]) = new RoutePattern[L0] {
    val pattern: PathMatcher[L0] = pmatcher

    def ->[R0 <: RouteHandler](rh: => R0): RouteDef[L0, R0] = new RouteDef[L0, R0] {
      val pattern = pmatcher
      def handler(segments: L0): R0 = rh
    }

    def ->[Fn0, R0 <: RouteHandler](f: Fn0)
                                   (implicit ftp: FnToProduct.Aux[Fn0, (L0) => R0]) =
      new RouteDef[L0, R0] {
        val pattern: PathMatcher[L0] = pmatcher
        def handler(segments: L0): R0 = ftp(f)(segments)
      }
  }
}


package myapp

import escalera.router._

import scala.annotation.implicitNotFound
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.window

//spandrel

/**
 * Created by zackangelo on 1/7/15.
 */
@JSExport
object HackerNews {

  import RouteMapping._
  import PathMatcher._

  case class StoriesRoute() extends RouteHandler
  case class StoryRoute(storyId: String) extends RouteHandler
  case class StoryIndexRoute(storyId: String) extends RouteHandler
  case class StoryCommentsRoute(storyId: String) extends RouteHandler
  case class StoryCommentRoute() extends RouteHandler
  case class StoryDescriptionRoute(storyId: String) extends RouteHandler
  case class UsersRoute() extends RouteHandler
  case class UserRoute(userId: String) extends RouteHandler

  import shapeless._
  import shapeless.ops.function.FnToProduct

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

  trait RouteDef[L <: HList, R <: RouteHandler] extends RouteMatcher {
    self =>
    val pattern: PathMatcher[L]

    def handler(segments: L): R

    def apply(path: String) = pattern(path) match {
      case Matched(segments, rest) => {
        window.console.log(s"path = $path, segments = $segments, rest = $rest")

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

    def then[RH <: RouteHandler](childFn: R => RouteMatcher) = new RouteDef[L, R] {
      val pattern = self.pattern
      def handler(segments: L) = self.handler(segments)
      override def children(rh: R) = Some(childFn(rh))
    }
  }

  object route2 {
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

  val router3 = route2("users") -> UsersRoute() then { users =>
    route2(Segment) -> (UserRoute(_))
  }

  @JSExport
  val router2 =
    route2("stories") -> StoriesRoute() then { stories =>
      route2(Segment) -> (StoryRoute(_)) then { story =>
        route2("comments") -> StoryCommentsRoute(story.storyId) then { comments =>
          route2(Segment) -> StoryCommentRoute()
        } or
        route2("description") -> StoryDescriptionRoute(story.storyId)
      }
    } or (route2("users") -> UsersRoute() then { users =>
      route2(Segment) -> (UserRoute(_))
    })


  def applyRouter(path: String, router: RouteMatcher, matched: List[RouteHandler]): List[RouteHandler] = {
    if (path.isEmpty) {
      matched
    } else {
      router.lift(path) match {
        case Some(RouteMatchResult(handler, Some(child), remain)) =>
          applyRouter(remain, child, handler :: matched)
        case Some(RouteMatchResult(handler, None, remain)) if remain.isEmpty =>
          handler :: matched
        case _ =>
          List.empty
      }
    }
  }

  @JSExport
  def applyRouter(path: String): List[RouteHandler] =
    applyRouter(path, router2, List.empty)
}


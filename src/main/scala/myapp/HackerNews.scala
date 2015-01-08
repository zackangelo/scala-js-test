package myapp

import escalera.router._
import shapeless.ops.function.FnToProduct

//spandrel

/**
 * Created by zackangelo on 1/7/15.
 */
object HackerNews {

  import RouteMapping._
  import PathMatcher._

  case class StoriesRoute() extends RouteHandler
  case class StoryRoute(storyId:String) extends RouteHandler
  case class StoryIndexRoute(storyId:String) extends RouteHandler
  case class StoryCommentsRoute(storyId:String) extends RouteHandler
  case class StoryDescriptionRoute(storyId:String) extends RouteHandler

  val path:String = "hello/world"

  import shapeless._

  trait RouteDefinition[L <: HList] {
    val pattern:PathMatcher[L]
    val handler:RouteHandler
    def or[H <: HList](nextRoute:RouteDefinition[H])
    def then[F](f:F)(implicit ftp: FnToProduct.Aux[F, L => RouteHandler]):RouteDefinition[L]
  }

  trait RoutePatternDefinition[L <: HList] {
    val pattern:PathMatcher[L]
    def to(dest:RouteHandler):RouteDefinition[L]
    def to[F](f:F)(implicit ftp: FnToProduct.Aux[F, L => RouteHandler]):RouteDefinition[L]
  }

  object route {
    def apply[L <: HList](pmatcher: PathMatcher[L]): RoutePatternDefinition[L] = ???
  }

  object index extends PathMatcher0 {
    override def apply(v1: String): MatchResult[HNil] = ???
  }

  implicit def route2def[L <: HList](rh:RouteHandler):RouteDefinition[L]

  route("stories") to StoriesRoute() then {
    route(Segment) to { (storyId:String) =>
      StoryRoute(storyId) then {
        route(index)         to StoryIndexRoute(storyId)        or
        route("comments")    to StoryCommentsRoute(storyId)     or
        route("description") to StoryDescriptionRoute(storyId)
      }
    }
  }
}

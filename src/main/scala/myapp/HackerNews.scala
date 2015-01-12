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
  import PathMatcher._
  import escalera.router.route

  case class StoriesRoute() extends RouteHandler
  case class StoryRoute(storyId: String) extends RouteHandler
  case class StoryIndexRoute(storyId: String) extends RouteHandler
  case class StoryCommentsRoute(storyId: String) extends RouteHandler
  case class StoryCommentRoute() extends RouteHandler
  case class StoryDescriptionRoute(storyId: String) extends RouteHandler
  case class UsersRoute() extends RouteHandler
  case class UserRoute(userId: String) extends RouteHandler

  @JSExport
  val router2 =
    route("stories") -> StoriesRoute() then { stories =>
      route(Segment) -> (StoryRoute(_)) then { story =>
        route("comments") -> StoryCommentsRoute(story.storyId) then { comments =>
          route(Segment) -> StoryCommentRoute()
        } or
        route("description") -> StoryDescriptionRoute(story.storyId)
      }
    } or (route("users") -> UsersRoute() then { users =>
      route(Segment) -> (UserRoute(_))
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
        case _ if path.isEmpty =>
          matched
        case _ =>
          List.empty
      }
    }
  }

  @JSExport
  def applyRouter(path: String): List[RouteHandler] =
    applyRouter(path, router2, List.empty)
}


package myapp

import escalera.Application
import escalera.router._
import scala.annotation.implicitNotFound
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.window

import scalatags.JsDom.all._

//spandrel

/**
 * Created by zackangelo on 1/7/15.
 */
@JSExport
object HackerNews extends Application {
  import PathMatcher._

  trait TestRoute extends Route[String] {
    import scala.concurrent.Future

    def state = Future.successful("Hello")
    def render(s:String) = Future.successful(div(
      `class`:="test-route",
      div(s)
    ).render)
  }

  case class StoriesRoute() extends TestRoute
  case class StoryRoute(storyId: String) extends TestRoute
  case class StoryIndexRoute(storyId: String) extends TestRoute
  case class StoryCommentsRoute(storyId: String) extends TestRoute
  case class StoryCommentRoute() extends TestRoute
  case class StoryDescriptionRoute(storyId: String) extends TestRoute
  case class UsersRoute() extends TestRoute
  case class UserRoute(userId: String) extends TestRoute

  @JSExport
  def root =
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
}


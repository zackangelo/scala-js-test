package escalera.router

import scala.annotation.tailrec
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.window
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.Future

trait Router {
  private[router] implicit val context = new RouterContext()

  def routesForUrl(url:String):List[Route] = {

    @tailrec
    def matchSegments(root:Route, segments:List[String], matched:List[Route]):List[Route] = {
      segments match {
        case next :: rest =>
          val nextRoute = context.children(root).find(_.path == next).getOrElse {
            throw new IllegalStateException(s"No route found for URL $url")
          }

          matchSegments(nextRoute, rest, matched :+ nextRoute)
        case Nil => matched
      }
    }

    if(url.isEmpty) {
      List(rootRoute)
    } else {
      matchSegments(rootRoute, url.split("/").toList, List(rootRoute))
    }
  }

  def handleUrlChange(url:String) = {
    window.console.log(s"Transitioning to URL $url...")

    val activeRoutes = context.activeRoutes.reverse
    val targetRoutes:List[Route] = routesForUrl(url)

    window.console.log(s"Routes for URL: $targetRoutes")

    val common = activeRoutes.zipWithIndex.takeWhile({ e =>
      val (r,i) = e
      i < targetRoutes.length && targetRoutes(i) == r
    }).map(_._1)

    window.console.log(s"Routes active $activeRoutes")
    window.console.log(s"Routes in common $common")

    val toEnter = targetRoutes.takeRight(targetRoutes.length - common.length)
    val toExit = activeRoutes.takeRight(activeRoutes.length - common.length)

    window.console.log(s"Routes to enter $toEnter")
    window.console.log(s"Routes to exit $toExit")

    toExit foreach {
      context.exit(_)
    }

    import scala.util.{ Success, Failure }

    context.enterAll(toEnter) andThen {
      case Success(_) => window.document.body.appendChild(context.rootElement)
      case Failure(ex) => throw ex
    }
  }

  def root:Route
  private lazy val rootRoute = root
}
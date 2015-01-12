package escalera.router

import scala.annotation.tailrec
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.window
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.Future
import scala.scalajs.js.annotation.JSExport

trait Router {
  private[router] implicit val context = new RouterContext()

  def applyRouter(path: String, router: RouteMatcher, matched: List[Route[_]]): List[Route[_]] = {
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

  def routesForUrl(path:String):List[Route[_]] =
    applyRouter(path, rootRoute, List.empty).reverse

  def handleUrlChange(url:String) = {
    window.console.log(s"Transitioning to URL $url...")

    val activeRoutes = context.activeRoutes.reverse
    val targetRoutes = routesForUrl(url)

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

  @JSExport
  def startRouting() = {
    window.console.log(s"Starting router for ${getClass.getName}...")

    window.onhashchange = { ev:Event =>
      handleUrlChange(window.location.hash.substring(1))
    }

    handleUrlChange(window.location.hash.substring(1))
  }

  def root:RouteMatcher

  private lazy val rootRoute = root
}
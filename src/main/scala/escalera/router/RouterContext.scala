package escalera.router

import org.scalajs.dom
import org.scalajs.dom.window
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Created by zackangelo on 1/7/15.
 */

case class ActiveRouteState(route:Route[_],
                            element:Option[dom.Node],
                            outlet:Option[dom.Node])

class RouterContext {
  import scala.collection.mutable
  import scalatags.JsDom.all._

  private[router] val active    = new mutable.Stack[ActiveRouteState]
  private[router] val rootElement = div(`class`:="application").render

  def activeRoutes:List[Route[_]] = active.map(_.route).toList
  def leaf:Route[_] = active.last.route

  def outletForFragment(el:dom.Element):Option[dom.Node] = Some(el)

  /**
   * Enters the route
   *
   * @param route
   * @return
   */
  def enter[S](route:Route[S]):Future[Unit] = {
    val leafOutlet = active.lastOption match {
      case Some(leaf) => leaf.outlet
      case None       => Some(rootElement) //no active routes, at root
    }

    window.console.log(s"Entering route $route...")

    for {
      m   <- route.state
      el  <- route.render(m)
    } yield {
      val node = leafOutlet map {
        _.appendChild(el)
      }

      val childOutlet = outletForFragment(el)

      active.push(ActiveRouteState(route, node, childOutlet))

      ()
    }
  }

  def enterAll(routes:List[Route[_]]):Future[Unit] = {
    routes match {
      case head :: Nil  => enter(head)
      case head :: tail => enter(head) flatMap(_ => enterAll(tail))
      case Nil => Future.successful(Unit)
    }
  }

  def exit(route:Route[_]):Unit = {
    window.console.log(s"Exiting route $route...")

    val state = active.pop()

    state.element.foreach { n =>
      n.parentNode.removeChild(n)
    }
  }
}

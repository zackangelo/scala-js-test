package escalera.router

import org.scalajs.dom
import org.scalajs.dom.window
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Created by zackangelo on 1/7/15.
 */

case class ActiveRouteState(route:Route,
                            element:Option[dom.Node],
                            outlet:Option[dom.Node],
                            children:List[Route])

class RouterContext {
  import scala.collection.mutable
  import scalatags.JsDom.all._

  private[router] val children  = mutable.Map.empty[Route, List[Route]]
  private[router] val parents   = mutable.Map.empty[Route, Route]
  private[router] val active    = new mutable.Stack[ActiveRouteState]

  private[router] val rootElement = div(`class`:="application").render

  def activeRoutes:List[Route] = active.map(_.route).toList
  def leaf:Route = active.last.route

  def outletForFragment(el:dom.Element):Option[dom.Node] = Some(el)

  /**
   * Enters the route
   *
   * @param route
   * @return
   */
  def enter(route:Route):Future[Unit] = {
    val leafOutlet = active.lastOption match {
      case Some(leaf) => leaf.outlet
      case None       => Some(rootElement) //no active routes, at root
    }

    window.console.log(s"Entering route $route...")

    for {
      m   <- route.model
      el  <- route.render(m)
    } yield {
      val node = leafOutlet map {
        _.appendChild(el)
      }

      val childOutlet = outletForFragment(el)

      active.push(ActiveRouteState(route, node, childOutlet, List.empty))

      ()
    }
  }

  def enterAll(routes:List[Route]):Future[Unit] = {
    routes match {
      case head :: Nil  => enter(head)
      case head :: tail => enter(head) flatMap(_ => enterAll(tail))
      case Nil => Future.successful(Unit)
    }
  }

  def exit(route:Route):Unit = {
    window.console.log(s"Exiting route $route...")

    val state = active.pop()

    state.element.foreach { n =>
      n.parentNode.removeChild(n)
    }
  }

  def register(route:Route, parent:Route) = {
    window.console.log(s"Registering route $route...")
    val childs = children.getOrElse(parent, List.empty[Route])
    children += (parent -> (childs :+ route))
    parents += (route -> parent)
  }
}

package zackangelo

import scala.annotation.tailrec
import scala.scalajs.js.annotation.JSExport
import scala.concurrent._
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.window
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.matching.Regex
import scalatags.JsDom.all._

case class Order(id:Int, customerName:String)
case class OrderItem(id:Int, orderId:Int, name:String, price:Double)

object Order {
  val All = Seq(Order(1, "Zack"), Order(2, "Nate"), Order(3, "Jon"))
  val AllItems = Seq(
    OrderItem(1, 1, "Roast Beef Sandwich", 2.00),
    OrderItem(2, 1, "Turkey Wrap", 3.50),
    OrderItem(3, 2, "Lettuce Wrap", 4.00),
    OrderItem(4, 3, "Hamburger", 5.00)
  )
}

trait PathSegment {
  def matches(s:String):Boolean
//  def /(ps:PathSegment):Path = this :: ps :: Nil
}

object Segment extends PathSegment {
  override def matches(s:String) = true
  def unapply(s:String):Seq[String] = Seq(s)
}

object IntSegment extends PathSegment {
  private def stringToInt(s:String):Option[Int] =
    try Some(s.toInt)
    catch {
      case ex: NumberFormatException => None
    }

  override def matches(s:String) = stringToInt(s).isDefined
}

case class StringPathSegment(string:String) extends PathSegment {
  override def matches(s:String):Boolean = string == s
}

trait Router {
  def root:Route
  def log = window.console.log _

  implicit def route2chainable[T <: Route](route:T):Chainable = new Chainable {
    def ~(sibling:Route):RouteChain = route :: sibling :: Nil
  }

  implicit def chain2chainable(chain:RouteChain):Chainable = new Chainable {
    def ~(sibling:Route):RouteChain = chain :+ sibling
  }

  implicit def route2chain[T <: Route](route:T):RouteChain = List(route)

  protected implicit val context = new RouterContext()

  @JSExport
  def startRouting() = {
    window.console.log("Starting router")

    window.onhashchange = { ev:Event =>
      handleUrlChange(window.location.hash.substring(1))
    }

    handleUrlChange(window.location.hash.substring(1))
  }

  def log(msg:String, params:Any*) = window.console.log _

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

  private lazy val rootRoute = root
}

trait Chainable {
  def ~(sibling:Route):RouteChain
}

case class ActiveRouteState(route:Route,
                            element:Option[dom.Node],
                            outlet:Option[dom.Node])

class RouterContext {
  import scala.collection.mutable
  import scalatags.JsDom.all._

  private[zackangelo] val children  = mutable.Map.empty[Route, List[Route]]
  private[zackangelo] val parents   = mutable.Map.empty[Route, Route]
  private[zackangelo] val active    = new mutable.Stack[ActiveRouteState]

  private[zackangelo] val rootElement = div(`class`:="application").render

  def activeRoutes:List[Route] = active.map(_.route).toList
  def leaf:Route = active.last.route

  def outletForFragment(el:dom.Element):Option[dom.Node] = Some(el)

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

      active.push(ActiveRouteState(route, node, childOutlet))

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

trait Route {
  type ModelType

  //TODO -> Path class -> List[PathElement]
  val path:String

  def model:Future[ModelType]
  def render(model:ModelType):Future[dom.Element] = ???
  def parent(implicit rcx:RouterContext):Route = rcx.parents(this)

  //these should just be methods? maybe an event handler instead?
  // i hate this method signature
  def actions:PartialFunction[Any,Unit] = ???

  def apply(children:RouteChain)
           (implicit rcx:RouterContext):Route = {

    children foreach {
      rcx.register(_, this)
    }

    this
  }
}

object ApplicationRoute extends Route {
  import scalatags.JsDom.all._

  type ModelType = Unit
  def model = Future.successful(())
  val path = ""

  override def render(model:Unit):Future[dom.Element] = Future {
    div(id:="application").render
  }
}



case class OrderByIdRoute(path:Path) extends Route {
  type ModelType = Order

  def model = Future.successful(Order.All.head)

  override def render(model:Order):Future[dom.Element] = Future {
    div("An Order").render
  }
}

case class MyNewOrderItemsRoute(path:Path) extends Route {
  import scalatags.JsDom.all._

  type ModelType = Seq[OrderItem]

  def model = Future.successful(Order.AllItems)

  override def render(model:Seq[OrderItem]):Future[dom.Element] = Future {
    div(`class`:="items",
      ol(
        model.map(item => li(`class`:="item", "#", item.id, " ", item.name))
      )
    ).render
  }
}

case class TextRoute(path:Path, content:String) extends Route {
  import scalatags.JsDom.all._

  type ModelType = String

  def model = Future.successful(content)

  override def render(model:String):Future[dom.Element] = Future {
    div(`class`:="text-route",
      "Some text: ", model
    ).render
  }
}

@JSExport
object AppRouter extends Router {
//  implicit def string2segment(s:String):PathSegment = StringPathSegment(s)
//  implicit def string2path(s:String):Path = List(StringPathSegment(s))
//  implicit def segment2path(ps:PathSegment):Path = List(ps)
//  implicit def path2segment(s:Path):PathSegment = new PathSegment {
//    override def /(ps:PathSegment):Path = s :+ ps
//    override def matches(s: String): Boolean = false
//  }

//  val path:Path = "orders" / IntSegment / "items"

  //TODO/Idea make child routes a function of parent route's state
  def root = ApplicationRoute {
    OrdersRoute("orders") {
      MyNewOrderItemsRoute("items") ~
      TextRoute("edit", "Editing an order") ~
      TextRoute("blah", "Blahing an order")
    } ~
    TextRoute("text", "texty text")
  }
}

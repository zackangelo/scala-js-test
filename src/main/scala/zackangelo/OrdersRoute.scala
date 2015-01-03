package zackangelo

import org.scalajs.dom
import scala.concurrent._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Created by zackangelo on 12/15/14.
 */
case class OrdersRoute(path:Path) extends Route {
  import scalatags.JsDom.all._

  type ModelType = Seq[Order]

  def model = Future.successful(Order.All)

  override def render(model:Seq[Order]):Future[dom.Element] = Future {
    div(
      `class`:="orders",
      div("An Order")
    ).render
  }
}

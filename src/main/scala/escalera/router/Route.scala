package escalera.router

import org.scalajs.dom
import scala.concurrent.Future

/**
 * Created by zackangelo on 1/7/15.
 */
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
}

package escalera.router

import shapeless._
import shapeless.ops.function._

trait RouteHandler {
  def apply(children: => Seq[RouteMapping[_]]) = ???
}

trait RouteMapping[F] {
  val path:PathMatcher[_]
  //    val handler:RouteHandler
  def ->(f:F) = ???
}

object RouteMapping {
  implicit def pm2mapping[F,H <: HList](m:PathMatcher[H])
                                       (implicit ftp: FnToProduct.Aux[F, H => RouteHandler]):RouteMapping[F] = {
    new RouteMapping[F] {
      val path = m
      override def ->(f:F) = ???
    }
  }
}


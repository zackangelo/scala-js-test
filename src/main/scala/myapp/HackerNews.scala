package myapp

import escalera.router._

import scala.annotation.implicitNotFound


//spandrel

/**
 * Created by zackangelo on 1/7/15.
 */
object HackerNews {

  import RouteMapping._
  import PathMatcher._

  case class StoriesRoute() extends RouteHandler
  case class StoryRoute(storyId:String) extends RouteHandler
  case class StoryIndexRoute(storyId:String) extends RouteHandler
  case class StoryCommentsRoute(storyId:String) extends RouteHandler
  case class StoryDescriptionRoute(storyId:String) extends RouteHandler

  val path:String = "hello/world"

  import shapeless._
  import shapeless.ops.hlist.Prepend
  import shapeless.ops.function.FnToProduct
  import shapeless.ops.function.FnFromProduct

  trait RouteDefinition {
    type RH <: RouteHandler
    type L <: HList

    val pattern:PathMatcher[L]
    def handler(segments:L):RH
    val children:List[RouteDefinition]

//    def or[H <: HList](nextRoute:RouteDefinition)
//    def then[F,Out](f:F)(implicit pp: Prepend.Aux[RH :: HNil, L, Out],
//                     ftp: FnToProduct.Aux[F, Out => RouteDefinition]):RouteDefinition
  }

//  trait RoutePatternDefinition {
//    type L <: HList
//    val pattern:PathMatcher[L]
//    def to[F,R <: RouteDefinition](f:F)(implicit ftp: FnToProduct.Aux[F, L => R]):RouteDefinition
//  }

//  object route {
//    def apply[L0 <: HList,R0 <: RouteDefinition,F0](pmatcher: PathMatcher[L0]): RoutePatternDefinition = new RoutePatternDefinition {
//      override type L = L0
//      override val pattern: PathMatcher[L] = pmatcher
//
//      override def to[F,R <: RouteDefinition](f: F)
//                                             (implicit ftp: FnToProduct.Aux[F, (L) => R]): RouteDefinition =
//        new RouteDefinition {
//          override type RH = R
//          override type L = L0
//
//          override val children: List[RouteDefinition] = List.empty
//          override def handler(segments:L):RH = ftp.apply(f)(segments)
//          override val pattern: PathMatcher[L] = pmatcher
//        }
//    }
//  }

  implicit def route2def[F,L <: HList](rh:RouteHandler):RouteDefinition = ???

  object route0 {
    def apply[L0 <: HList, R0 <: RouteDefinition, Fn0](pmatcher:PathMatcher[L0])
                                                      (handlerFn:Fn0)
                                                      (implicit ftp: FnToProduct.Aux[Fn0, (L0) => R0]) = ???
  }

//  route0("stories") { () => StoriesRoute() }
//  route0("stories" / Segment) { (storyId:String) => route2def(StoryRoute(storyId)) }


  trait RouteAux[L] {
    def to[Fn0,R0 <: RouteHandler](f:Fn0)(implicit ftp: FnToProduct.Aux[Fn0, (L) => R0]):RouteAux2[R0]
//    def to[R <: RouteHandler](rd:R):RouteAux2[L,Fn]
  }

  trait RouteAux2[PR <: RouteHandler] {
    def then[R1 <: RouteHandler](childrenFn:PR => RouteAux2[R1]): RouteAux2[_]
  }

  trait RouteAux3[PR <: RouteHandler] {
    def children(parentRoute:PR):RouteAux2[_]
  }

  trait RoutePattern[L <: HList] {
    val pattern:PathMatcher[L]
    def ->[R0 <: RouteHandler](rh: => R0):RouteDef[L,R0]
    def ->[Fn0,R0 <: RouteHandler](f:Fn0)
                                  (implicit ftp: FnToProduct.Aux[Fn0, (L) => R0]):RouteDef[L,R0]
  }

  type RouteMatcher = PartialFunction[String, Tuple2[RouteHandler, String]]

  trait RouteDef[L <: HList,R <: RouteHandler] extends RouteMatcher { self =>
    val pattern:PathMatcher[L]

    def handler(segments:L):R

    def apply(path:String) = pattern.apply(path) match {
      case Matched(segments,rest) => (handler(segments),rest)
      case _ => throw new IllegalStateException(s"Matcher is invalid for path $path")
    }

    def isDefinedAt(path:String) = pattern.apply(path) match {
      case Matched(_,_) => true
      case _            => false
    }

    def or(next:RouteMatcher) = this.orElse(next)

    def children(rh:R):Option[RouteMatcher] = None

    def then[RH <: RouteHandler](childFn:R => RouteMatcher) = new RouteDef[L,R] {
      val pattern = self.pattern
      def handler(segments:L) = self.handler(segments)
      override def children(rh:R) = Some(childFn(rh))
    }
  }

  object route2 {
    def apply[L0 <: HList](pmatcher:PathMatcher[L0]) = new RoutePattern[L0] {
      val pattern:PathMatcher[L0] = pmatcher

      def ->[R0 <: RouteHandler](rh: => R0):RouteDef[L0,R0] = new RouteDef[L0,R0] {
        val pattern = pmatcher
        def handler(segments:L0):R0 = rh
      }

      def ->[Fn0,R0 <: RouteHandler](f:Fn0)
                                    (implicit ftp: FnToProduct.Aux[Fn0, (L0) => R0]) =
        new RouteDef[L0,R0] {
          val pattern: PathMatcher[L0] = pmatcher
          def handler(segments: L0): R0 = ftp(f)(segments)
        }
    }
  }

  val rr =
    route2("stories") -> StoriesRoute() then { stories =>
      route2(Segment) -> (StoryRoute(_)) then { story =>
        route2(index)         -> StoryIndexRoute(story.storyId)
        route2("comments")    -> StoryCommentsRoute(story.storyId) or
        route2("description") -> StoryDescriptionRoute(story.storyId)
      }
    }


  rr("comments/2")

  object route1 {
    def apply[L0 <: HList](pmatcher:PathMatcher[L0]) = new RouteAux[L0] {
      def to[Fn1,R0 <: RouteHandler](f:Fn1)
                              (implicit ftp: FnToProduct.Aux[Fn1, (L0) => R0]) = new RouteAux2[R0] {
        def then[R1 <: RouteHandler](childrenFn:R0 => RouteAux2[R1]) = this
      }
    }
  }

  val r =
    route1("stories") to (() => StoriesRoute()) then { (stories:StoriesRoute) =>
      route1(Segment) to { (storyId:String) => StoryRoute(storyId) } then { story =>
        route1("comments") to { () => StoryCommentsRoute(story.storyId) }
      }
    }

  route1("stories" / Segment) to { (storyId:String) => StoryRoute(storyId) }
  route1("stories" / Segment) to (StoryRoute(_))
  route1("stories" / Segment / Segment) to { (storyId:String, param2:String) =>
    StoryRoute(storyId)
  }

//  object route {
//    def apply[L0 <: HList, Fn0](pmatcher:PathMatcher[L0])
//                               (handlerFn:Fn0)
//                               (implicit ftp: FnToProduct.Aux[Fn0, (L0) => RouteDefinition]) = ???
//  }
//
//  val rdf = route2def(StoriesRoute())
//
//  route("stories")(rdf)

  object index extends PathMatcher0 {
    override def apply(v1: String): MatchResult[HNil] = ???
  }

//  val root = route("stories") to StoriesRoute() then {
//    route(Segment) to { (storyId:String) =>
//      route2def(StoryRoute(storyId)) then {
//        route("comments")    to StoryCommentsRoute(storyId)
//      }
//    }
//  }
//  val root =
//    route("stories") to StoriesRoute() then { (stories:StoriesRoute) =>
//      route(Segment) to { (storyId:String) => StoryRoute(storyId) } then {
//        route("comments") to StoryCommentsRoute(storyId)
//      }
//    }

  //should yield StoriesRoute :: StoryRoute(200) :: StoryCommentsRoute(200)
  val in = "stories/200/comments"

//  def matchRoutes(path:String, siblings:List[RouteDefinition[_]],
//                  matched:List[RouteDefinition[_]]):Option[List[RouteDefinition[_]]] = {
//    siblings collectFirst {
//      case Matched(extracted, remaining) =>
//    }
//
//    if(path.isEmpty) {
//      Some(matched)
//    } else {
//      siblings.map((s: RouteDefinition[_]) => (s, s.pattern.apply(path))) collectFirst {
//        case (rd, Matched(ex, rem)) => matchRoutes(rem, rd.children, rd :: matched)
//      }.flatten
//    }
//  }
}

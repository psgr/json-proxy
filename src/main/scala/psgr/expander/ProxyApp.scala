package psgr.expander

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server._
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}

object ProxyApp extends App {
  implicit val system = ActorSystem("proto-proxy")
  implicit val mat = ActorFlowMaterializer()
  implicit val ec = system.dispatcher

  val local9000 = Http(system).outgoingConnection("localhost", 9000)

  val proxy: Route = Route { context =>

    val request = context.request

    request.uri.query

    Source.single(request)
      .via(local9000)
      .runWith(Sink.head)
      .flatMap { r =>

      context.complete(r)
    }
  }

  Http(system).bindAndHandle(proxy, interface = "localhost", port = 1080)
}

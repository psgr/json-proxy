package psgr.proxy

import akka.http.scaladsl.marshalling.{ PredefinedToEntityMarshallers, ToEntityMarshaller }
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.unmarshalling.{ FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers }
import akka.stream.FlowMaterializer
import play.api.libs.json.{ JsError, Json, Reads, Writes }
import scala.concurrent.ExecutionContext

object PlayJsonMarshalling extends PlayJsonMarshalling

/** Play JSON integration for Akka HTTP (un)marshalling. */
trait PlayJsonMarshalling {

  /** `FromEntityUnmarshaller` for `application/json` depending on a Play JSON `Reads`. */
  implicit def unmarshaller[A](implicit reads: Reads[A], ec: ExecutionContext, mat: FlowMaterializer): FromEntityUnmarshaller[A] =
    PredefinedFromEntityUnmarshallers.stringUnmarshaller
      .forContentTypes(MediaTypes.`application/json`)
      .map(s => reads.reads(Json.parse(s)).recoverTotal(error => sys.error(JsError.toJson(error).toString())))

  /** `ToEntityMarshaller` to `application/json` depending on a Play JSON `Writes`. */
  implicit def marshaller[A](implicit writes: Writes[A]): ToEntityMarshaller[A] =
    PredefinedToEntityMarshallers.stringMarshaller(MediaTypes.`application/json`)
      .compose(a => writes.writes(a).toString())
}

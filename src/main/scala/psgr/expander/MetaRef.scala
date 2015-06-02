package psgr.expander

import play.api.libs.json._

case class MetaRef(href: String, mimeType: Option[String])

object MetaRef {
  implicit val f = (__ \ "meta").format(Json.format[MetaRef])
}
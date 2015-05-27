package psgr.proxy

import play.api.libs.json._

import scala.concurrent.Future

case class FieldExpander(field: String, subs: => Seq[FieldExpander]) {
  lazy val tested = subs

  def apply(json: JsObject, resolver: MetaRef => Future[JsObject]): Future[JsObject] = {
    (json \ field).toOption


    path(json).headOption.toList flatMap {
      case j: JsObject =>
        Seq(j)

      case JsArray(ja) =>
        ja

      case j =>
        Seq.empty[JsValue]
    }

    ???
  }
}

object FieldExpander {
  def parseExpandFields(exp: List[String]): Map[String,List[String]] = exp.map {e =>
    val pos = e.indexOf('.')
    if(pos > 0) {
      val (field, subs) = e.splitAt(pos)
      Some(field -> subs.drop(1))
    } else None
  }.collect {
    case Some(n) => n
  }.groupBy(_._1).mapValues(_.map(_._2))

  def parse(exp: List[String]): Seq[FieldExpander] = parseExpandFields(exp).map {
    case (k, v) => FieldExpander(k, parse(v))
  }.toSeq
}
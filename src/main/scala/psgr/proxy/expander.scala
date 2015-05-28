package psgr.proxy

import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

trait JsonResolver extends (MetaRef => Future[JsObject]) {
  implicit def ctx: ExecutionContext = scala.concurrent.ExecutionContext.global
}

object JsonExpander {
  def apply(obj: JsObject, fields: List[String]): JsonFieldsExpander = JsonFieldsExpander(obj, parse(fields))
  
  def apply(fields: List[String]): FieldsExpander = FieldsExpander(parse(fields))

  def parse(exp: List[String]): Map[String, List[String]] = exp.map { e =>
      val pos = e.indexOf('.')
      if (pos > 0) {
        val (field, subs) = e.splitAt(pos)
        field -> (subs.drop(1) :: Nil)
      } else e -> List.empty[String]
    }.groupBy(_._1).mapValues(_.flatMap(_._2))
}

case class JsonExpander(obj: JsObject) {
  def apply(fields: List[String]) = 
    JsonFieldsExpander(
      obj, 
      JsonExpander.parse(fields)
    )
}

case class FieldsExpander(fields: Map[String, List[String]]) {
  def apply(obj: JsObject) = JsonFieldsExpander(obj, fields)
}

case class JsonFieldsExpander(obj: JsObject, fields: Map[String, List[String]]) {
  private def resolved(implicit resolver: JsonResolver): JsonResolvedExpander =
    JsonResolvedExpander(
      obj.validate[MetaRef].asOpt.filter(_ => obj.keys.size == 1).fold(Future successful obj)(resolver), 
      fields
    )

  def expand(implicit resolver: JsonResolver): Future[JsObject] = 
    resolved.expand
}

case class JsonResolvedExpander(objF: Future[JsObject], fields: Map[String, List[String]]) {
  def expand(implicit resolver: JsonResolver): Future[JsObject] = {
    import resolver.ctx
    objF.flatMap {obj =>
      Future.traverse(obj.keys.intersect(fields.keySet)) {f =>
        val fieldHandler = JsonExpander.parse(fields(f))
        val valueF: Future[JsValue] = (obj \ f).get match {
          case o:JsObject =>
            JsonFieldsExpander(o, fieldHandler).expand
          case JsArray(ja) =>
            Future.traverse(ja.seq){
              case o: JsObject =>
                JsonFieldsExpander(o, fieldHandler).expand
              case j =>
                Future.successful(j)
            }.map(JsArray)
          case j =>
            Future successful j
        }
        valueF.map(f -> _).map{v => println(v); v}
      }.map(_.foldLeft(obj)(_ + _))
    }
  }
}
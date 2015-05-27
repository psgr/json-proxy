package psgr.proxy

import org.specs2._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.concurrent.Future

class JsonLensSpec extends mutable.Specification {
  /*
  что нам нужно от линз?
  - сделать линзу по строке
  - линза: жсон =Ю список(реф, сеттер)
  - получаем список референсов
  - дёргаем для всех запрос
  - поочерёдно приставляем результаты к жсону
   */

  type RefResolver = MetaRef => Future[JsObject]

  type RefSetter = (JsObject, JsObject) => JsObject
  type RefGetter = JsObject => Seq[(MetaRef, RefSetter)]

  def build(jpath: String): RefGetter = {
    val path = jpath.split('.').foldLeft[JsPath](__)(_ \ _)

    println("PATH = " + path.toJsonString)

    (source) => {
      path(source).headOption.toList.flatMap {
        case j: JsObject =>
          Seq(j)
        case JsArray(js) =>
          js
        case _ =>
          Seq.empty[JsValue]
      }.flatMap(_.validate[MetaRef].asOpt.toList).zipWithIndex.map {
        case (r, i) =>
          (r, (o: JsObject, v: JsObject) => {
            val na = path(o).headOption map {
              case JsArray(arr) =>
                val ja = JsArray(arr.toIndexedSeq.updated(i, arr(i).asInstanceOf[JsObject] ++ v))
                println("ja = " + ja)
                ja
              case ov => ov.asInstanceOf[JsObject] ++ v
            }

            val n = o.validate((path.json.prune ~ path.json.put(na.getOrElse(v))).reduce).getOrElse(o)
            println(s"$i: $o -> $n")
            n
          })
      }

    }
  }

  val objMixin = Json.obj(
    "field" -> "exists"
  )

  "json lens" should {

    "get sub obj" in {
      val nestedJsonObj = Json.obj(
        "meta" -> Json.obj("href" -> "/parent"),
        "sub" -> Json.obj(
          "meta" -> Json.obj(
            "href" -> "/parent/sub"
          )
        )
      )

      val objGetter: RefGetter = build("sub")

      val l = objGetter(nestedJsonObj)

      l.size must_== 1
      l.head._1.href must_== "/parent/sub"

      l.head._2(nestedJsonObj, objMixin) must_== Json.obj(
        "meta" -> Json.obj("href" -> "/parent"),
        "sub" -> Json.obj(
          "meta" -> Json.obj(
            "href" -> "/parent/sub"
          ),
          "field" -> "exists"
        )
      )
    }

    "get items" in {
      val itemsJson = Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "items" -> Seq(
          Json.obj("meta" -> Json.obj("href" -> "/list/0")),
          Json.obj("meta" -> Json.obj("href" -> "/list/1")),
          Json.obj("meta" -> Json.obj("href" -> "/list/2"))
        )
      )

      val objGetter: RefGetter = build("items")


      val l = objGetter(itemsJson)

      l.size must_== 3
      l.head._1.href must_== "/list/0"

      val res = l.map(_._2).foldLeft(itemsJson) {
        case (json, setter) =>
          setter(json, objMixin)
      }

      res must_== Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "items" -> Seq(
          Json.obj("meta" -> Json.obj("href" -> "/list/0"),
            "field" -> "exists"),
          Json.obj("meta" -> Json.obj("href" -> "/list/1"),
            "field" -> "exists"),
          Json.obj("meta" -> Json.obj("href" -> "/list/2"),
            "field" -> "exists")
        )
      )
    }

    "get sub with point" in {
      val nestedJsonObj = Json.obj(
        "meta" -> Json.obj("href" -> "/parent"),
        "sub" -> Json.obj(
          "meta" -> Json.obj(
            "href" -> "/parent/sub"
          ),
          "child" -> Json.obj(
            "meta" -> Json.obj(
              "href" -> "/parent/sub/child"
            )
          )
        )
      )

      val objGetter: RefGetter = build("sub.child")

      val l = objGetter(nestedJsonObj)

      l.size must_== 1
      l.head._1.href must_== "/parent/sub/child"

      l.head._2(nestedJsonObj, objMixin) must_== Json.obj(
        "meta" -> Json.obj("href" -> "/parent"),
        "sub" -> Json.obj(
          "meta" -> Json.obj(
            "href" -> "/parent/sub"
          ),
          "child" -> Json.obj(
            "meta" -> Json.obj(
              "href" -> "/parent/sub/child"
            ),
            "field" -> "exists"
          )
        )
      )
    }

    "get items with point" in {
      val itemsJson = Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "sub" -> Json.obj(
          "items" -> Seq(
            Json.obj("meta" -> Json.obj("href" -> "/list/0")),
            Json.obj("meta" -> Json.obj("href" -> "/list/1")),
            Json.obj("meta" -> Json.obj("href" -> "/list/2"))
          ))
      )

      val objGetter: RefGetter = build("sub.items")


      val l = objGetter(itemsJson)

      l.size must_== 3
      l.head._1.href must_== "/list/0"

      val res = l.map(_._2).foldLeft(itemsJson) {
        case (json, setter) =>
          setter(json, objMixin)
      }

      res must_== Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "sub" -> Json.obj("items" -> Seq(
          Json.obj("meta" -> Json.obj("href" -> "/list/0"),
            "field" -> "exists"),
          Json.obj("meta" -> Json.obj("href" -> "/list/1"),
            "field" -> "exists"),
          Json.obj("meta" -> Json.obj("href" -> "/list/2"),
            "field" -> "exists")
        ))
      )
    }


    "get items sub with point" in {
      val itemsJson = Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "sub" -> Json.obj(
          "items" -> Seq(
            Json.obj("child" ->
              Json.obj("meta" -> Json.obj("href" -> "/list/0"))),
            Json.obj("child" ->
              Json.obj("meta" -> Json.obj("href" -> "/list/1"))),
            Json.obj("child" ->
              Json.obj("meta" -> Json.obj("href" -> "/list/2")))
          ))
      )

      val objGetter: RefGetter = build("sub.items")


      val l = objGetter(itemsJson)

      l.size must_== 3
      l.head._1.href must_== "/list/0"

      val res = l.map(_._2).foldLeft(itemsJson) {
        case (json, setter) =>
          setter(json, objMixin)
      }

      res must_== Json.obj(
        "meta" -> Json.obj("href" -> "/list"),
        "sub" -> Json.obj("items" -> Seq(
          Json.obj("child" ->
            Json.obj("meta" -> Json.obj("href" -> "/list/0"),
              "field" -> "exists")),
          Json.obj("child" ->
            Json.obj("meta" -> Json.obj("href" -> "/list/1"),
              "field" -> "exists")),
          Json.obj("child" ->
            Json.obj("meta" -> Json.obj("href" -> "/list/2"),
              "field" -> "exists"))
        ))
      )
    }

    "perform complex loading" in {
      def m = (href: String) => Json.obj("meta" -> Json.obj("href" -> href))

      val a = m("/a") ++ Json.obj("sub" -> m("/b"))

      val resolve = Map(
        "/b" -> (m("/b") ++ Json.obj("child" -> Json.obj("intern" -> m("/c"), "keep" -> "this", "other" -> m("/f")))),
        "/c" -> (m("/c") ++ Json.obj("items" -> Seq(m("/d/0"), m("/d/1")))),
        "/d/0" -> (m("/d/0") ++ Json.obj("field" -> m("/e"))),
        "/d/1" -> (m("/d/1") ++ Json.obj("ext" -> true)),
        "/e" -> (m("/e") ++ Json.obj("ext" -> true)),
        "/f" -> (m("/f") ++ Json.obj("foo" -> "bar"))
      )

      val expand = "sub.child.intern.items.field,sub.unexistent,sub.child.other"

      expand.split(',').toList.map {e =>
        val pos = e.indexOf('.')
        if(pos > 0) {
          val (field, subs) = e.splitAt(pos)
          Some(field -> subs.drop(1))
        } else None
      }.collect {
        case Some(n) => n
      }.groupBy(_._1)






      FieldExpander.parseExpandFields(expand.split(',').toList) must_== Map("sub" -> List("child.intern.items.field", "unexistent", "child.other"))
    }
  }
}

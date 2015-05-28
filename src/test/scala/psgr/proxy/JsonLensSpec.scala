package psgr.proxy

import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FutureMatchers
import play.api.libs.json._

import scala.concurrent.Future

class JsonLensSpec extends mutable.Specification with FutureMatchers {

  implicit val ee = ExecutionEnv.fromGlobalExecutionContext

  val objMixin = Json.obj(
    "field" -> "exists"
  )

  case class MapResolver(data: Map[String,JsObject]) extends JsonResolver {
    override def apply(v1: MetaRef): Future[JsObject] = Future successful data(v1.href)
  }

  case class MixinResolver(mixin: JsObject) extends JsonResolver {
    override def apply(v1: MetaRef): Future[JsObject] = Future.successful(Json.toJson(v1).asInstanceOf[JsObject] ++ mixin)
  }

  val mixinResolver = MixinResolver(objMixin)

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

      implicit val r = mixinResolver

      val exp = JsonExpander("sub" :: Nil)(nestedJsonObj)

      println("exp = "+exp)

      JsonExpander("sub" :: Nil)(nestedJsonObj).expand must beEqualTo(Json.obj(
        "meta" -> Json.obj("href" -> "/parent"),
        "sub" -> Json.obj(
          "meta" -> Json.obj(
            "href" -> "/parent/sub"
          ),
          "field" -> "exists"
        )
      )).await
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

      implicit val r = mixinResolver

      JsonExpander("items" :: Nil)(itemsJson).expand must beEqualTo(
        Json.obj(
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
      ).await
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

      implicit val r = mixinResolver

      JsonExpander("sub.child" :: Nil)(nestedJsonObj).expand must beEqualTo(
        Json.obj(
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
      ).await
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

      implicit val r = mixinResolver

      JsonExpander("sub.items" :: Nil)(itemsJson).expand must beEqualTo(
        Json.obj(
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
      ).await
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

      implicit val r = mixinResolver

      JsonExpander("sub.items.child" :: Nil)(itemsJson).expand must beEqualTo(
        Json.obj(
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
      ).await

    }

    "perform complex loading" in {
      def m = (href: String) => Json.obj("meta" -> Json.obj("href" -> href))

      val a = m("/a") ++ Json.obj("sub" -> m("/b"))

      val resolve = Map(
        "/b" -> (m("/b") ++ Json.obj("child" -> Json.obj("intern" -> m("/c"), "keep" -> "this", "other" -> m("/f")))),
        "/c" -> (m("/c") ++ Json.obj("items" -> Seq(m("/d/0"), m("/d/1")))),
        "/d/0" -> (m("/d/0") ++ Json.obj("field" -> m("/e"))),
        "/d/1" -> (m("/d/1") ++ Json.obj("ext-d1" -> true)),
        "/e" -> (m("/e") ++ Json.obj("ext-e" -> true)),
        "/f" -> (m("/f") ++ Json.obj("foo" -> "bar"))
      )

      implicit val r = MapResolver(resolve)

      val expand = "sub.child.intern.items.field,sub.unexistent,sub.child.other"

      JsonExpander.parse(expand.split(',').toList) must_== Map("sub" -> List("child.intern.items.field", "unexistent", "child.other"))

      JsonExpander(a, expand.split(',').toList).expand.map(_.toString()) must beEqualTo(
        """{"meta":{"href":"/a"},"sub":{"meta":{"href":"/b"},"child":{"intern":{"meta":{"href":"/c"},"items":[{"meta":{"href":"/d/0"},"field":{"meta":{"href":"/e"},"ext-e":true}},{"meta":{"href":"/d/1"},"ext-d1":true}]},"keep":"this","other":{"meta":{"href":"/f"},"foo":"bar"}}}}"""
      ).await

    }
  }
}

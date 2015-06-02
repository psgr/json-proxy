package psgr.expander

import akka.http.scaladsl.model.Uri
import org.specs2._

class RequestFilterSpec extends mutable.Specification{
  "request filter" should {
    "be empty" in {
      RequestFilter.read(Uri.Query()) must_== RequestFilter()
    }
  }
}

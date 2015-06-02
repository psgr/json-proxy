package psgr.expander

import akka.http.scaladsl.model.Uri

case class RequestFilter(expand: Seq[String] = Seq.empty, fields: Seq[String] = Seq.empty, omit: Seq[String] = Seq.empty) {
  def isEmpty = expand.isEmpty && fields.isEmpty && omit.isEmpty

  def nonEmpty = !isEmpty
}

object RequestFilter {
  def read(q: Uri.Query): RequestFilter = {
      RequestFilter(expand = q.getAll("expand").flatMap(_.split(",")), fields = q.getAll("fields").flatMap(_.split(",")), omit = q.getAll("omit").flatMap(_.split(",")))
  }
}

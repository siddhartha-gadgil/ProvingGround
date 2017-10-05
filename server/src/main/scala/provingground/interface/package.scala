// import akka.http.scaladsl.marshalling.{Marshaller, _}
// import akka.http.scaladsl.model.MediaType
// import akka.http.scaladsl.model.MediaTypes._
// import play.twirl.api.{Html, Txt, Xml}

// package object provingground {
//
//   /** Twirl marshallers for Xml, Html and Txt mediatypes */
//   implicit val twirlHtmlMarshaller = twirlMarshaller[Html](`text/html`)
//   implicit val twirlTxtMarshaller  = twirlMarshaller[Txt](`text/plain`)
//   implicit val twirlXmlMarshaller  = twirlMarshaller[Xml](`text/xml`)
//
//   def twirlMarshaller[A](contentType: MediaType): ToEntityMarshaller[A] = {
//     Marshaller.StringMarshaller.wrap(contentType)(_.toString)
//   }
// }

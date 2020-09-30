package provingground.interface
import org.mongodb.scala.bson.collection.immutable._
import provingground._, HoTT._, induction._ 
import ConciseTermJson._


object TermBson{
    def  jsToDoc(js: ujson.Value) = Document(ujson.write(js))

    def docToJs(doc: Document) = ujson.read(doc.toJson())

    val termToBson = termToJson.map[Document](jsToDoc, docToJs)

    def bsonToTerm(
        inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
        indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
          (_) => None) = (doc: Document) => jsonToTerm(inds, indexedInds)(ujson.read(doc.toJson()))
}
// Compactifying Json, Saving and Loading
import provingground._ , learning._, interface._, translation._, HoTT._, induction._
val input = """{
  "id": "c730433b-082c-4984-9d66-855c243266f0",
  "name": "Foo",
  "counts": [1, 2, 3],
  "values": {
    "bar": true,
    "baz": 100,
    "qux": ["a", "b"]
  }
}"""
val cj = new CompactJson()
val data = ujson.read(input)
val (index, _) = cj.getIndex(data)
cj.getValue(index) == data
val bs = CompactJson.save(cj)
val cj1 = CompactJson.load(bs)
cj1.keyBuffer
cj1.getValue(12) == data

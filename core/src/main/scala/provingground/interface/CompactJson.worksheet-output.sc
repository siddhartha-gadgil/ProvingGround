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
// input: String = """{
//   "id": "c730433b-082c-4984-9d66-855c243266f0",
//   "name": "Foo",
//   "counts": [1, 2, 3],
//   "values": {
//     "bar": true,
//     "baz": 100,
//     "qux": ["a", "b"]
//   }
// }"""
val cj = new CompactJson()
// cj: CompactJson = provingground.interface.CompactJson@65250979
val data = ujson.read(input)
// data: ujson.Value.Value = Obj(
//   value = LinkedHashMap(
//     "id" -> Str(value = "c730433b-082c-4984-9d66-855c243266f0"),
//     "name" -> Str(value = "Foo"),
//     "counts" -> Arr(value = ArrayBuffer(Num(value = 1.0), Num(value = 2.0), Num(value = 3.0))),
//     "values" -> Obj(
//       value = LinkedHashMap(
//         "bar" -> true,
//         "baz" -> Num(value = 100.0),
//         "qux" -> Arr(value = ArrayBuffer(Str(value = "a"), Str(value = "b")))
//       )
//     )
//   )
// )
val (index, _) = cj.getIndex(data)
// index: Int = 12
cj.getValue(index) == data
// res0: Boolean = true
val bs = CompactJson.save(cj)
// bs: Array[Byte] = Array(
//   7,
//   2,
//   105,
//   100,
//   4,
//   110,
//   97,
//   109,
//   101,
//   6,
//   99,
//   111,
//   117,
//   110,
//   116,
//   115,
//   6,
//   118,
//   97,
//   108,
//   117,
//   101,
//   115,
//   3,
//   98,
//   97,
//   114,
//   3,
//   98,
//   97,
//   122,
//   3,
//   113,
//   117,
//   120,
//   5,
//   36,
//   99,
//   0,
//   0,
//   0,
//   0,
//   55,
//   0,
//   0,
//   0,
//   1,
//   51,
// ...
val cj1 = CompactJson.load(bs)
// cj1: CompactJson = provingground.interface.CompactJson@683d60e0
cj1.keyBuffer
// res1: collection.mutable.ArrayBuffer[String] = ArrayBuffer("id", "name", "counts", "values", "bar", "baz", "qux")
cj1.getValue(12) == data
// res2: Boolean = true


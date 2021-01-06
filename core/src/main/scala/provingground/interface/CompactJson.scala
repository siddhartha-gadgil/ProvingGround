package provingground.interface

import scala.collection.parallel._, immutable.ParVector
import CompactJson._
import scala.collection.mutable.ArrayBuffer
import ujson.Str
import ujson.Obj
import ujson.Arr
import ujson.Num
import ujson.Bool

object CompactJson {
  type Desc = (Byte, Array[(Byte, Int)])
}

class CompactJson(preMap : ujson.Value => ujson.Value = identity,  L: Int) {
  val keyBuff: ArrayBuffer[String] = ArrayBuffer()

  def keyByte(key: String): Byte =
    keyBuff.zipWithIndex
      .find(_._1 == key)
      .map(_._2)
      .getOrElse {
        keyBuff.append(key)
        keyBuff.size - 1
      }
      .toByte

  var compactValues: ParVector[Desc] = ParVector()

  var descMap: ParMap[Desc, Int] = ParMap()

  def appendDesc(desc: Desc): Int = {
    compactValues = compactValues :+ desc
    val index = compactValues.size - 1
    descMap = descMap.updated(desc, index)
    index
  }

  def getIndex(js: ujson.Value, knownNew: Boolean): (Int, Boolean) = {
    val (desc, b) = getDesc(js, knownNew)
    if (knownNew || b) (appendDesc(desc), true)
    else
      descMap.get(desc).map(_ -> false).getOrElse(appendDesc(desc) -> true)
  }

  def getDesc(js: ujson.Value, knownNew: Boolean): (Desc, Boolean) = preMap(js) match {
    case Str(value) => 
        val data = value.toCharArray().map(_.toInt).zipWithIndex.map{case (x, n) => (n.toByte, x)}
        (((3 + value.length + (2 * L)).toByte, data), false)
    case Obj(value) =>
      val dataFat = value.toArray.map {
        case (key, x) => (keyByte(key), getIndex(x, knownNew))
      }
      val someNew = dataFat.exists(_._2._2)
      val data    = dataFat.map { case (b, (n, _)) => (b, n) }
      (((3 + value.size + L).toByte, data), someNew)
    case Arr(value) =>
      val dataFat = value.zipWithIndex.map {
        case (x, n) => (n.toByte, getIndex(x, knownNew))
      }
      val someNew = dataFat.exists(_._2._2)
      val data    = dataFat.map { case (b, (n, _)) => (b, n) }.toArray
      (((3 + value.length).toByte, data), someNew)
    case Num(value) => ((2, Array(0.toByte -> value.toInt)), false)
    case b: Bool =>
      if (b.value) ((0.toByte, Array()), false)
      else ((1.toByte, Array()), false) // type 0 is "true" and 1 is "false"
    case ujson.Null =>
      throw new IllegalArgumentException("obtained null json value")
  }
}

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

  def intToBytes(n: Int, l: Int = 4): Array[Byte] = {
    val base = BigInt(n).toByteArray
    Array.fill(l - base.length)(0.toByte) ++ base
  }

  def descToBytes(d: Desc): Array[Byte] =
    Array(d._1, d._2.length.toByte) ++ d._2.flatMap {
      case (b, n) => b +: intToBytes(n)
    }

    // head description and the rest
  def headDesc(bs: Array[Byte]) : (Desc, Array[Byte]) = {
    val tp = bs(0)
    val l = bs(1)
    val (data, tail) = bs.drop(2).splitAt(5 * l)
    val descData = data.grouped(5).map{
        case gp => (gp.head, BigInt(gp.tail).toInt)
    }.to(Array)
    ((tp, descData), tail)
  }

  @annotation.tailrec
  def allDescs(bs: Array[Byte], accum: Array[Desc]): Array[Desc] = 
    if (bs.isEmpty) accum else {
        val (head, tail) = headDesc(bs)
        allDescs(tail, accum :+ head)
    }
}

class CompactJson(
    preMap: ujson.Value => ujson.Value = identity,
    postMap: ujson.Value => ujson.Value = identity
) {
  val keyBuffer: ArrayBuffer[String] = ArrayBuffer()

  def keyByte(key: String): Byte =
    keyBuffer.zipWithIndex
      .find(_._1 == key)
      .map(_._2)
      .getOrElse {
        keyBuffer.append(key)
        keyBuffer.size - 1
      }
      .toByte

  var descriptions: ParVector[Desc] = ParVector()

  var descMap: ParMap[Desc, Int] = ParMap()

  def appendDesc(desc: Desc): Int = {
    descriptions = descriptions :+ desc
    val index = descriptions.size - 1
    descMap = descMap.updated(desc, index)
    index
  }

  def getIndex(js: ujson.Value, knownNew: Boolean): (Int, Boolean) = {
    val (desc, b) = getDesc(js, knownNew)
    if (knownNew || b) (appendDesc(desc), true)
    else
      descMap.get(desc).map(_ -> false).getOrElse(appendDesc(desc) -> true)
  }

  def getDesc(js: ujson.Value, knownNew: Boolean): (Desc, Boolean) =
    preMap(js) match {
      case Str(value) =>
        val data = value.toCharArray().map(_.toInt).zipWithIndex.map {
          case (x, n) => (n.toByte, x)
        }
        ((5.toByte, data), false)
      case Obj(value) =>
        val dataFat = value.toArray.map {
          case (key, x) => (keyByte(key), getIndex(x, knownNew))
        }
        val someNew = dataFat.exists(_._2._2)
        val data    = dataFat.map { case (b, (n, _)) => (b, n) }
        ((4.toByte, data), someNew)
      case Arr(value) =>
        val dataFat = value.zipWithIndex.map {
          case (x, n) => (n.toByte, getIndex(x, knownNew))
        }
        val someNew = dataFat.exists(_._2._2)
        val data    = dataFat.map { case (b, (n, _)) => (b, n) }.toArray
        ((3.toByte, data), someNew)
      case Num(value) => ((2, Array(0.toByte -> value.toInt)), false)
      case b: Bool =>
        if (b.value) ((0.toByte, Array()), false)
        else ((1.toByte, Array()), false) // type 0 is "true" and 1 is "false"
      case ujson.Null =>
        throw new IllegalArgumentException("obtained null json value")
    }

  def getValue(index: Int): ujson.Value = postMap(descValue(descriptions(index)))

  def descValue(desc: Desc): ujson.Value = {
    desc match {
      case (0, _)             => ujson.False
      case (1, _)             => ujson.True
      case (2, Array((_, k))) => ujson.Num(k)
      case (3, data)          => ujson.Arr.from(data.map { case (_, n) => getValue(n) })
      case (4, data) =>
        val kv = data.map { case (k, n) => (keyBuffer(k), getValue(n)) }
        ujson.Obj.from(kv)
      case (5, data) => data.map(_._2.toChar).mkString("")
    }
  }
}

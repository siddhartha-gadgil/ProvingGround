package provingground

import LeanExportElem._
import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object LeanIO {
  def readData(file: Path) = Data.readAll(read.lines(file))
  
  def readDefs(file: Path) = {
    val lines = read.lines(file)
    
    val dat = Data.readAll(lines)
    
    dat.readDefs(lines)
  }
  
  def pickleDefs(file: Path, outputDir: Path) = {
    val defs = readDefs(file)
    val lines = defs map (_.depPickle)
    val out = outputDir / file.name
    rm(out)
    lines map((l) => {write(out, l); l})
  }
  
  def futPickleDefs(file: Path, outputDir: Path) = 
    Future(pickleDefs(file, outputDir))  
    
  def makeDefs(inDir: Path = cwd / 'data / 'leanlibrary, outDir: Path = cwd / 'data / 'leandefs) = {
    val files = ls(inDir) filter (_.ext == "export")
    (files map ((f) => (f.name, futPickleDefs(f, outDir)))).toMap
  }
  
  def snapshot(fd: Map[String,Future[Vector[String]]]) = 
    ((fd.values map (_.value)).flatten map (_.toOption)).flatten.toVector.flatten
    
  def recallDefs(defDir: Path = cwd / 'data / 'leandefs) = {
    ls(defDir).toVector flatMap ((f) => 
      read.lines(f) map (_.split("\t").toList)
      )
  }
}
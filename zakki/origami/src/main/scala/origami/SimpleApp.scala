package origami

import spire.implicits._
import spire.math._
import scala.collection.immutable.Vector
import java.io.File

object Fold {
  def fold(src: Vertex, dst: Vertex, pt: Vertex): Vertex= {
    val c = (src + dst) / 2;
    return c;
  }
}

import java.nio.file.Files;

object SimpleApp extends App {
  import Fold._
  println("ok")

  for (f <- new File("../../problems").listFiles() if f.getName().endsWith(".dat")) {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    println(p)
  }
  
  println(fold(Vertex(0, 0), Vertex(0, 1), Vertex(0, 1)))
}

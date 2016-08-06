package origami

import spire.implicits._
import spire.math._
import scala.collection.immutable.Vector
import java.io.File
import origami.math._

object Fold {
  def fold(src: Vertex, dst: Vertex, pt: Vertex): Vertex = {
    val c = (src + dst) / 2;
    return c;
  }
}

import java.nio.file.Files;

object CreateProblemImages extends App {
  import Fold._
  println("ok")

  for (f <- new File("../../problems").listFiles() if f.getName().endsWith(".dat")) {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    println(p)
    Visualizer.saveImage(p, f.getAbsolutePath() + ".png")
  }
}

object CreateSolutionImages extends App {
  for (f <- new File("../../answers").listFiles() if f.getName().endsWith(".dat")) {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val s = new Reader(lines).readSolution()
    println(s)
    Visualizer.saveImage(s, f.getAbsolutePath() + ".png")
  }
}

object SimpleApp extends App {
  import Fold._
  println("ok")
  val f = new File(args(0))
  val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
  val p = new Reader(lines).readProblem
  println(p)

  for (e <- Util.collectLine(p)) {
    println(e)
  }
  val facetCandidates = Util.facets(p)
  for (f <- facetCandidates) {
    println(f)
  }
}

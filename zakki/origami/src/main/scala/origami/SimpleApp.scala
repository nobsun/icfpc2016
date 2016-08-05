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
  def createImage(f: File): Unit = {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    println(p)
    Visualizer.saveImage(p, f.getAbsolutePath() + ".png")
  }

  for (a <- args) {
    val f = new File(a)
    if (f.isFile()) {
      createImage(f)
    } else {
      for (ff <- f.listFiles() if ff.isFile() && ff.getName.endsWith(".dat")) {
        createImage(ff)
      }
    }
  }
}

object CreateSolutionImages extends App {
  def createImage(f: File) = {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val s = new Reader(lines).readSolution()
    println(s)
    Visualizer.saveImage(s, f.getAbsolutePath() + ".png")
  }

  for (a <- args) {
    val f = new File(a)
    println(f.getAbsolutePath)
    if (f.isFile()) {
      createImage(f)
    } else {
      for (ff <- f.listFiles() if (ff.isFile() && ff.getName.endsWith(".dat"))) {
        createImage(ff)
      }
    }
  }
}

object SimpleApp extends App {
  def solve(solver: Solver, f: File, num: Int): Boolean = {
    println(f)
    val queue = new scala.collection.mutable.Queue[solver.Node]
    queue ++= solver.createRoot()
    var i = 0
    while (i < num && !queue.isEmpty) {
      if (i % 100 == 0)
        print(".")
      val n = queue.dequeue()
      val fs = n.toFacets()
      val t = solver.isSquare(fs)
      if (t == Solver.OK) {
        val es = n.eset.keys.toVector
        val p1 = Problem(fs.map(f => Polygon(f.vertices)), es)
        println(p1)
        println("#####")
        val name = f.getAbsolutePath + ".ans" + i + ".png"
        println(">> " + name)
        Visualizer.saveImage(p1, name)
        println(n)
        return true
      }
      if (t == Solver.LESS) {
        for (c <- n.expand()) {
          //println(c)
          queue += c
          //val fs = c.toFacets().map(f => Polygon(f.vertices))
          //val fs = c.toFacets().flatMap(f => Vector(Polygon(f.vertices), Polygon(f.vertices.reverse)))
        }
      }
      i += 1
    }
    return false
  }

  if (true) {
    for (i <- 1 to 101) {
      val f = new File("../../problems/", i + ".dat")
      if (f.exists()) {
        val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
        val p = new Reader(lines).readProblem
        val solver = Solver(p)
        solve(solver, f, 500)
      }
    }
    //solve("13.dat", 500)
  } else {
    println("ok")
    val f = new File(args(0))
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    println(p)

    val solver = Solver(p)
    for (e <- solver.edges) {
      println(e)
    }
    for (f <- solver.facets) {
      println(f)
      println(solver.toFacet(f))
    }
    var i = 0
    for (n <- solver.createRoot()) {
      println("*r********************")
      println(n)
      //val es = n.eset.keys.toVector

      val es = solver._edges
      val p0 = Problem(n.toFacets().map(f => Polygon(f.vertices)), es)
      Visualizer.saveImage(p0, f.getName() + "." + i + ".png")
      var j = 0
      //if (false)
      for (c <- n.expand()) {
        println(c)
        val fs = c.toFacets().map(f => Polygon(f.vertices))
        //val fs = c.toFacets().flatMap(f => Vector(Polygon(f.vertices), Polygon(f.vertices.reverse)))
        val es = c.eset.keys.toVector
        val p1 = Problem(fs, es)
        println(p1)
        val name = f.getName() + "." + i + "-" + j + ".png"
        println(">> " + name)
        Visualizer.saveImage(p1, name)
        if (solver.isSquare(c.toFacets()) == Solver.OK) {
          println("#####")
          println(c)
        }
        j += 1
      }
      i += 1
    }
  }
}

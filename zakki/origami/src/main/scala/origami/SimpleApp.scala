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
    println("> " + f)
    //if (!new File(f.getAbsoluteFile + ".png").exists())
    try {
      val p = new Reader(lines).readProblem
      println(p)
      Visualizer.saveImage(p, new File("problems-img", f.getName() + ".png"))
      SimpleApp.dumpFacet(Solver(p), new File(f.getName + ".facet"))
    } catch {
      case e: Throwable =>
        Console.err.println("parse error " + f)
    }
  }

  for (a <- args) {
    val f = new File(a)
    if (f.isFile()) {
      if (f.getName.endsWith(".dat"))
      createImage(f)
    } else {
      for (ff <- f.listFiles().par if ff.isFile() && ff.getName.endsWith(".dat")) {
        createImage(ff)
      }
    }
  }
}

object CreateSolutionImages extends App {
  def createImage(f: File) = {
    try {
      val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
      val s = new Reader(lines).readSolution()
      println(s)
      Visualizer.saveImage(s, new File("answers-img", f.getName() + ".png"))
    } catch {
      case e: Throwable =>
        Console.err.println("parse error " + f)
    }
  }

  for (a <- args) {
    val f = new File(a)
    println(f.getAbsolutePath)
    if (f.isFile()) {
      createImage(f)
    } else {
      for (ff <- f.listFiles().par if (ff.isFile() && ff.getName.endsWith(".dat"))) {
        createImage(ff)
      }
    }
  }
}

object SimpleApp extends App {
  def solve(solver: Solver, f: File, num: Int): Boolean = {
    println(f)
    val queue = new scala.collection.mutable.Queue[solver.Node]
    //val queue = new scala.collection.mutable.Stack[solver.Node]
    queue ++= solver.createRoot()
    if (true) {
      val hs = solver.hint()
      println("#####")
      for (i <- 0 until hs.length) {
        val n = hs(i)
        val fs = n.toFacets()
        val es = n.eset.keys.toVector
        val p1 = Problem(fs.map(f => Polygon(f.vertices)), es)
        println(p1)

        for (f <- n.tfacets; f2 <- n.tfacets if f != f2) {
          if (intersect(f.vertices, f2.vertices))
            println("  intersect " + f.vertices + "/" + f2.vertices)
        }
        val t = solver.isSquare(fs)
        if (t == Solver.MORE) {
          println(">> " + t)
        } else {
          val name = f.getAbsolutePath + ".hint." + i + ".png"
          println(">> " + name)
          Visualizer.saveImage(p1, new File("out", f.getName() + ".png"))
        }
      }

      queue ++= hs
    }
    //queue.pushAll(solver.createRoot())
    var i = 0
    while (i < num && !queue.isEmpty) {
      if (i % 100 == 0)
        print(".")
      val n = queue.dequeue()
      //val n = queue.pop()
      val fs = n.toFacets()
      val t = solver.isSquare(fs)
      if (t == Solver.OK) {
        val es = n.eset.keys.toVector
        val p1 = Problem(fs.map(f => Polygon(f.vertices)), es)
        println(p1)
        println("#####")
        val name = f.getAbsolutePath + ".ans" + i + ".png"
        println(">> " + name)
        Visualizer.saveImage(p1, new File("out", name))
        println(n)
        return true
      }
      if (t == Solver.LESS) {
        for (c <- n.expand()) {
          //println(c)
          queue += c
          //queue.push(c)
          //val fs = c.toFacets().map(f => Polygon(f.vertices))
          //val fs = c.toFacets().flatMap(f => Vector(Polygon(f.vertices), Polygon(f.vertices.reverse)))
        }
      }
      i += 1
    }
    if (queue.isEmpty)
      println("not found")
    else
      println("exceed limit")
    return false
  }

  def dumpFacet(solver: Solver, dir: File) {
    val es = solver._edges
    dir.mkdir()
    for (i <- 0 until solver.facets.length) {
      val facet = solver.facets(i)
      println(solver.toFacet(facet))
      val name = i + ".png"
      println(">> " + name)
      val pf = Problem(Vector(Polygon(solver.toFacet(facet).vertices)), es)
      Visualizer.saveImage(pf, new File(dir, name))
    }
  }

  def solve(f: File) {
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    val solver = Solver(p)

    dumpFacet(solver, new File(f.getName + ".facet"))
    solve(solver, f, 500)
  }

  if (true) {
    solve(new File("../../problems/9.dat"))
    //solve(new File("../../problems/38.dat"))
    System.exit(0)
    val files = (for (
      f <- new File("../../problems/").listFiles() if f.getName.endsWith(".dat")
    ) yield f).toList
    files.par.foreach { f =>
      try {
        solve(f)
      } catch {
        case e =>
          e.printStackTrace()
      }
    }
    /*
    for (i <- 1 to 101) {
      val f = new File("../../problems/", i + ".dat")
      if (f.exists()) {
        solve(f)
      }
    }
    */
    //solve("13.dat", 500)
  } else {
    println("ok")
    val f = new File("../../problems/35.dat") //new File(args(0))
    val lines = Files.readAllLines(f.toPath()).toArray(Array[String]())
    val p = new Reader(lines).readProblem
    println(p)

    val solver = Solver(p)
    for (e <- solver.edges) {
      println(e)
    }
    dumpFacet(solver, f)
    //System.exit(1)
    var i = 0
    for (n <- solver.createRoot()) {
      println("*r********************")
      println(n)
      //val es = n.eset.keys.toVector

      val es = solver._edges
      val p0 = Problem(n.toFacets().map(f => Polygon(f.vertices)), es)
      Visualizer.saveImage(p, new File("out", f.getName() + "." + i + ".png"))
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
        Visualizer.saveImage(p1, new File("out", name))
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

package origami

import spire.implicits._
import spire.math._
import scala.collection.immutable.Vector
import java.awt.image.BufferedImage
import java.awt.geom._
import java.awt.Color
import java.awt.Graphics2D
import javax.imageio.ImageIO
import java.io.File
import java.util.regex.Pattern.Dot
import origami.math._

case class Facet(vertices: Vector[Vertex])

object Solver {
  def areaOf(p: Problem): (Rational, Rational, Rational, Rational) = {
    val area = new RArea(p.polygon(0).vertices(0))

    for (poly <- p.polygon; v <- poly.vertices) {
      area.update(v)
    }
    (area.minX, area.minY, area.maxX, area.maxY)
  }

  def collectLine(p: Problem): Vector[Edge] = {
    var edges: List[Edge] = Nil
    val pe = for (p <- p.polygon; i <- 0 until p.vertices.length) {
      val pa = p.vertices(i)
      val pb = p.vertices((i + 1) % p.vertices.length)
      if (!edges.exists(_.equals(pa, pb)))
        edges ::= Edge(pa, pb)
    }
    for (e <- p.edges) {
      if (!edges.exists(_.equals(e.a, e.b)))
        edges ::= e
    }
    edges.toVector
  }

  def facets(p: Problem) = {
    val edges = collectLine(p)
    val map = edges.flatMap { e => List((e.a, e), (e.b, e)) }
      .groupBy(t => t._1)
      .mapValues(e => e.map(_._2))
    var facets: Map[Set[Vertex], Facet] = Map()

    def next(e: Edge, edges: Set[Edge], vs: List[Vertex]): Unit = {
      //println("next " + e + " " + edges + "# " + vs);
      val ep = vs.head
      for (n <- map(ep) if !edges.contains(n)) {
        //println("iter " + n + " " + e + " " + vs)
        val (cp, np) =
          if (n.a == ep) {
            (n.a, n.b)
          } else {
            (n.b, n.a)
          }
        if (vs.contains(np)) {
          if (vs.length > 2 && (vs.last == np)) {
            //println("add " + n + vs)
            facets += (vs.toSet -> Facet(vs.toVector))
          } else {
            //println("skip " + np + vs.last + " ## " + vs)
          }
        } else {
          next(n, edges + n, np :: vs)
        }
      }
    }
    for (e <- edges) {
      //println("#start " + e + " # " + e.a);
      next(e, Set(e), List(e.a, e.b))
      //println("#start " + e + " # " + e.b);
      next(e, Set(e), List(e.b, e.a))
    }
    facets.values.toVector
  }

  def isSquareLike(vs: Seq[Vertex]): Boolean = {
    val v0 :: v1 :: _ = vs
    //ignore scale
    val t = createTrans(Edge(v0, v1),
      Edge(Vertex(Rational(0), Rational(0)), Vertex(Rational(1), Rational(0))))
    val vs1 = vs.map(v => t.transform(v))
    val a = new RArea(vs1)
    if (a.width != a.height) {
      //println(a)
      return false
    }
    if (!vs1.exists(p => p.x == a.minX && p.y == a.minY)) {
      //println(vs1)
      //println("no minmin " + a.minX + ", " + a.minY)
      return false
    }
    if (!vs1.exists(p => p.x == a.maxX && p.y == a.minY)) {
      //println(vs1)
      //println("no maxmin " + a.maxX + ", " + a.minY)
      return false
    }
    if (!vs1.exists(p => p.x == a.minX && p.y == a.maxY)) {
      //println(vs1)
      //println("no minmax " + a.minX + ", " + a.maxY)
      return false
    }
    if (!vs1.exists(p => p.x == a.maxX && p.y == a.maxY)) {
      //println(vs1)
      //println("no maxmax " + a.maxX + ", " + a.maxY)
      return false
    }
    return true
  }

  def isSquare(fs: List[Facet]): Boolean = {
    val vs = fs.flatMap { f => f.vertices }.sortBy { _.x }
    if (vs.length < 4)
      return false
    if (!isSquareLike(vs))
      return false
    true
  }
}

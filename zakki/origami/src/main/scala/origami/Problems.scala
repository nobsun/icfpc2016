package origami

import spire.implicits._
import spire.math._
import origami.math._

case class Polygon(vertices: Vector[Vertex])
case class Edge(a: Vertex, b: Vertex) {
  def equals(pa: Vertex, pb: Vertex): Boolean = {
    (a == pa && b == pb) || (a == pb && b == pa)
  }
  override def equals(o: Any): Boolean = {
    o match {
      case e: Edge => equals(e.a, e.b)
      case _       => false
    }

  }
  override def hashCode(): Int = a.hashCode() ^ b.hashCode()
}

case class Problem(polygon: Vector[Polygon], edges: Vector[Edge])

case class Facet(vertices: Vector[Vertex])


class Reader(lines: Array[String]) {
  var pos = 0;

  def readLine(): String = {
    val l = lines(pos)
    pos += 1
    l
  }

  def readIntLine(): Int = readLine().toInt;

  def readVertexLine() = toVertex(readLine())
  def readEdge() = toEdge(readLine())

  def toVertex(s: String): Vertex = {
    val tokens = s.split(",")
    if (tokens.length != 2) throw new IllegalArgumentException()
    Vertex(Rational(tokens(0)), Rational(tokens(1)))
  }

  def toEdge(s: String): Edge = {
    val tokens = s.split(" ")
    if (tokens.length != 2) throw new IllegalArgumentException()
    Edge(toVertex(tokens(0)), toVertex(tokens(1)))
  }

  def readPolygon(): Polygon = {
    val n = readIntLine()
    val pts = (for (i <- 0 until n)
      yield readVertexLine()).toVector
    Polygon(pts)
  }

  def readProblem(): Problem = {
    val n = readIntLine()
    val polygon = (for (i <- 0 until n)
      yield readPolygon()).toVector
    val ne = readIntLine()
    val edge = (for (i <- 0 until ne)
      yield readEdge()).toVector

    Problem(polygon, edge)
  }

  def readFacet(): SFacet = {
    val tokens = readLine().split(" ")
    val n = tokens(0).toInt
    if (tokens.length != n + 1) throw new IllegalArgumentException()
    val pts = (for (i <- 1 to n)
      yield tokens(i).toInt).toVector
    SFacet(pts)
  }

  def readSolution(): Solution = {
    val n = readIntLine()
    val src = (for (i <- 0 until n)
      yield readVertexLine()).toVector
    val ne = readIntLine()
    val facets = (for (i <- 0 until ne)
      yield readFacet()).toVector
    val dst = (for (i <- 0 until n)
      yield readVertexLine()).toVector
    Solution(src, facets, dst)
  }
}

case class SFacet(vertices: Vector[Int])

case class Solution(source: Vector[Vertex],
                    facets: Vector[SFacet],
                    destination: Vector[Vertex]) {

}

object Util {
  def areaOf(p: Problem): (Rational, Rational, Rational, Rational) = {
    val area = new RArea(p.polygon(0).vertices(0))

    for (poly <- p.polygon; v <- poly.vertices) {
      area.update(v)
    }
    for (e <- p.edges) {
      area.update(e.a)
      area.update(e.b)
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

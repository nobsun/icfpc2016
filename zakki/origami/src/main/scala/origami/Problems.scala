package origami

import spire.implicits._
import spire.math._
import scala.collection.immutable.Vector

case class Vertex(x: Rational, y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))

  def +(v: Vertex): Vertex = Vertex(x + v.x, y + v.y)
  def /(s: Rational): Vertex = Vertex(x /s,  y / s)
}

case class Polygon(vertices: Vector[Vertex])
case class Edge(a: Vertex, b: Vertex)

case class Problem(polygon: Vector[Polygon], edges: Vector[Edge])

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
      yield readVertexLine()
    ).toVector
    Polygon(pts)
  }
  
  def readProblem(): Problem = {
    val n = readIntLine()
    val polygon = (for (i <- 0 until n)
      yield readPolygon()
    ).toVector
    val ne = readIntLine()
    val edge = (for (i <- 0 until ne)
      yield readEdge()
    ).toVector

    Problem(polygon, edge)
  }
}

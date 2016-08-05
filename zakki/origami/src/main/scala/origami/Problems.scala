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
import spire.math.RationalNumber
import spire.math.RationalNumber

case class Vertex(x: Rational, y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))

  def +(v: Vertex): Vertex = Vertex(x + v.x, y + v.y)
  def -(v: Vertex): Vertex = Vertex(x - v.x, y - v.y)
  def *(s: Rational): Vertex = Vertex(x * s, y * s)
  def /(s: Rational): Vertex = Vertex(x / s, y / s)
  def dot(v: Vertex): Rational = x * v.x + y * v.y

  def toShortString() = x + "," + y
}

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

case class Facet(vertices: Vector[Vertex])

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
}

class RArea(x: Rational, y: Rational) {
  var minX: Rational = x
  var maxX: Rational = x
  var minY: Rational = y
  var maxY: Rational = y

  def this(v: Vertex) {
    this(v.x, v.y)
  }

  def this(vs: Seq[Vertex]) {
    this(vs(0).x, vs(0).y)
    for (v <- vs)
      update(v)
  }

  def width = maxX - minX
  def height = maxY - minY

  def update(v: Vertex): Unit = {
    if (minX > v.x) minX = v.x
    if (maxX < v.x) maxX = v.x
    if (minY > v.y) minY = v.y
    if (maxY < v.y) maxY = v.y
  }
}

object Util {
  def areaOf(p: Problem): (Rational, Rational, Rational, Rational) = {
    val area = new RArea(p.polygon(0).vertices(0))
    for (poly <- p.polygon; v <- poly.vertices) {
      area.update(v)
    }
    (area.minX, area.minY, area.maxX, area.maxY)
  }

  def isInner(v: Vertex, polygon: Vector[Vertex]) = {
    var count = 0
    for (i <- 0 until polygon.length) {
      val pa = polygon(i)
      val pb = polygon((i + 1) % polygon.length)
      //val dx = pb.x - pa.x
      //val dy = pb.y - pb.y

      // 上向きの辺。点Pがy軸方向について、始点と終点の間にある。ただし、終点は含まない。(ルール1)
      if (((pa.y <= v.y) && (pb.y > v.y))
        // 下向きの辺。点Pがy軸方向について、始点と終点の間にある。ただし、始点は含まない。(ルール2)
        || ((pa.y > v.y) && (pb.y <= v.y))) {
        // ルール1,ルール2を確認することで、ルール3も確認できている。
        // 辺は点pよりも右側にある。ただし、重ならない。(ルール4)
        // 辺が点pと同じ高さになる位置を特定し、その時のxの値と点pのxの値を比較する。
        val vt = (v.y - pa.y) / (pb.y - pa.y)
        if (v.x < (pa.x + (vt * (pb.x - v.x)))) {
          count += 1
          //println(i, v, pa, pb)
        }
      }
    }
    //println("result " + count)
    count % 2 == 1
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

  case class Transform(o: Vertex, ax: Vertex, ay: Vertex,
                       o1: Vertex, ax1: Vertex, ay1: Vertex, s: Rational) {
    def transform(p: Vertex): Vertex = {
      val p0 = p - o
      val x = p0.dot(ax)
      val y = p0.dot(ay)
      (ax1 * x + ay1 * y + o1) / s
    }
  }

  def createTrans(from: Edge, to: Edge): Transform = {
    val ax = from.b - from.a
    val ay = Vertex(ax.y, -ax.x)
    val ax1 = to.b - to.a
    val ay1 = Vertex(ax1.y, -ax1.x)
    val l2 = ax dot ax
    Transform(from.a, ax, ay, to.a, ax1, ay1, l2)
  }

  def transform(from: Edge, to: Edge, p: Vertex): Vertex = {
    createTrans(from, to).transform(p)
  }

  def _transform(from: Edge, to: Edge, p: Vertex): Vertex = {
    //val ox = to.a.x - from.a.x
    //val oy = to.a.y - from.a.y
    val p0 = p - from.a // / |ax|
    val ax = from.b - from.a
    val ay = Vertex(ax.y, -ax.x)
    val x = p0.dot(ax)
    val y = p0.dot(ay)

    val ax1 = to.b - to.a // / |ax1|
    val ay1 = Vertex(ax1.y, -ax1.x)

    val l2 = ax dot ax // |ax1| / |ax1|

    (ax1 * x + ay1 * y + to.a) / l2
  }
}

object Visualizer {
  def saveImage(p: Problem, filename: String) = {
    val img = drawImage(p)
    ImageIO.write(img, "png", new File(filename))
  }

  def drawImage(p: Problem) = {
    val size = 500
    val pad = 40
    val (minX, minY, maxX, maxY) = Util.areaOf(p)
    val w_ = maxX - minX
    val h_ = maxY - minY
    val l = max(w_, h_)
    val w = l
    val h = l
    val path = new GeneralPath(Path2D.WIND_EVEN_ODD)

    val img = new BufferedImage(size, size, BufferedImage.TYPE_3BYTE_BGR)
    val g = img.getGraphics().asInstanceOf[Graphics2D]
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size, size)

    g.setColor(Color.RED)
    g.drawLine(0, 0, size, size)

    def getx(x: Rational) = ((x - minX) / w).toDouble * (size - pad * 2) + pad
    def gety(y: Rational) = ((maxY - y) / h).toDouble * (size - pad * 2) + pad

    for (poly <- p.polygon) {
      var start = true
      for (v <- poly.vertices) {
        val x = getx(v.x)
        val y = gety(v.y)
        if (start) {
          path.moveTo(x, y)
          start = false
        } else {
          path.lineTo(x, y)
        }
      }
      path.closePath()
    }
    g.setColor(Color.LIGHT_GRAY)
    g.fill(path)

    for (e <- p.edges) {
      val x1 = getx(e.a.x)
      val y1 = gety(e.a.y)
      val x2 = getx(e.b.x)
      val y2 = gety(e.b.y)
      g.setColor(Color.BLACK)
      g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)

      g.setColor(Color.CYAN.darker())
      g.drawString(e.a.toShortString(), x1.toInt - 10, y1.toInt + 5)
      g.drawString(e.b.toShortString(), x2.toInt - 10, y2.toInt + 5)
    }

    for (poly <- p.polygon; v <- poly.vertices) {
      val x = getx(v.x)
      val y = gety(v.y)
      g.setColor(Color.GREEN.darker())
      g.drawString(v.toShortString(), x.toInt - 10, y.toInt + 5)
    }

    g.dispose()
    img
  }
}


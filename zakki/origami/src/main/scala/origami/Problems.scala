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

case class Vertex(x: Rational, y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))

  def +(v: Vertex): Vertex = Vertex(x + v.x, y + v.y)
  def /(s: Rational): Vertex = Vertex(x / s, y / s)
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

object Util {
  def areaOf(p: Problem): (Rational, Rational, Rational, Rational) = {
    var minX = p.polygon(0).vertices(0).x
    var maxX = p.polygon(0).vertices(0).x
    var minY = p.polygon(0).vertices(0).y
    var maxY = p.polygon(0).vertices(0).y
    for (poly <- p.polygon; v <- poly.vertices) {
      if (minX > v.x) minX = v.x
      if (maxX < v.x) maxX = v.x
      if (minY > v.y) minY = v.y
      if (maxY < v.y) maxY = v.y
    }
    (minX, minY, maxX, maxY)
  }
}

object Visualizer {
  def saveImage(p: Problem, filename: String) = {
    val img = drawImage(p)
    ImageIO.write(img, "png", new File(filename))
  }

  def drawImage(p: Problem) = {
    val size = 500
    val pad = 10
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

    for (poly <- p.polygon) {
      var start = true
      for (v <- poly.vertices) {
        val x = ((v.x - minX) / w).toDouble * (size - pad * 2) + pad
        val y = ((v.y - minY) / h).toDouble * (size - pad * 2) + pad
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

    g.setColor(Color.BLACK)
    for (e <- p.edges) {
      val x1 = ((e.a.x - minX) / w).toDouble * (size - pad * 2) + pad
      val y1 = ((e.a.y - minY) / h).toDouble * (size - pad * 2) + pad
      val x2 = ((e.b.x - minX) / w).toDouble * (size - pad * 2) + pad
      val y2 = ((e.b.y - minY) / h).toDouble * (size - pad * 2) + pad
      g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
    g.dispose()
    img
  }
}


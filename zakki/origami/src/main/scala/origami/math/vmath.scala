package origami.math


import spire.implicits._
import spire.math._

case class Vertex(x: Rational, y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))

  def +(v: Vertex): Vertex = Vertex(x + v.x, y + v.y)
  def -(v: Vertex): Vertex = Vertex(x - v.x, y - v.y)
  def *(s: Rational): Vertex = Vertex(x * s, y * s)
  def /(s: Rational): Vertex = Vertex(x / s, y / s)
  def dot(v: Vertex): Rational = x * v.x + y * v.y

  def toShortString() = x + "," + y
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

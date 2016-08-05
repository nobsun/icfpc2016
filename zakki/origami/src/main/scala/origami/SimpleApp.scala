package origami

import spire.implicits._
import spire.math._

case class Vertex(x: Rational, y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))

  def +(v: Vertex): Vertex = Vertex(x + v.x, y + v.y)
  def /(s: Rational): Vertex = Vertex(x /s,  y / s)
}


object Fold {
  def fold(src: Vertex, dst: Vertex, pt: Vertex): Vertex= {
    val c = (src + dst) / 2;
    return c;
  }
}

object SimpleApp extends App {
  import Fold._
  println("ok")

  println(fold(Vertex(0, 0), Vertex(0, 1), Vertex(0, 1)))
}

package origami

import spire.implicits._
import spire.math._
package object math {

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

package math {
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

  case class Transform(o: Vertex, ax: Vertex, ay: Vertex,
                       o1: Vertex, ax1: Vertex, ay1: Vertex, s: Rational) {
    def transform(p: Vertex): Vertex = {
      val p0 = p - o
      val x = p0.dot(ax)
      val y = p0.dot(ay)
      (ax1 * x + ay1 * y + o1) / s
    }
  }
}

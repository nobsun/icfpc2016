package origami

import spire.implicits._
import spire.math._

package object math {

  def crossPoint(ap: Edge, bp: Edge): Option[Vertex] =
    crossPoint(ap.a, ap.b, bp.a, bp.b)
  def crossPoint(apa: Vertex, apb: Vertex, bpa: Vertex, bpb: Vertex): Option[Vertex] = {
    val x21 = apb.x - apa.x
    val y21 = apb.y - apa.y
    val x43 = bpb.x - bpa.x
    val y43 = bpb.y - bpa.y
    val x31 = bpa.x - apa.x
    val y31 = bpa.y - apa.y

    val det = x43 * y21 - y43 * x21;
    if (det == 0)
      return None;

    val s = (x43 * y31 - y43 * x31) / det;
    val t = (x21 * y31 - y21 * x31) / det;
    if (s < 0 || s > 1 || t < 0 || t > 1)
      return None;

    val x = apa.x + x21 * s
    val y = apa.y + y21 * s
    Some(Vertex(x, y))
  }

  def normalize(a: Vector[Vertex]): Vector[Vertex] = {
    def f(a: Vector[Vertex], i: Int): Vector[Vertex] = {
      if (i < a.length) {
        val a0 = a(i)
        val a1 = a((i + 1) % a.length)
        val a2 = a((i + 2) % a.length)
        val d0 = a1 - a0
        val d1 = a2 - a1
        val d = d0 dot d1
        if (d * d == (d0 dot d0) * (d1 dot d1))
          f(a.filter { v => v != a1 }, i)
        else
          f(a, i + 1)
      } else {
        a
      }
    }
    f(a, 0)
  }

  def intersect(_a: Vector[Vertex], _b: Vector[Vertex]): Boolean = {
    val a = normalize(_a)
    val b = normalize(_b)
    for (i <- 0 until a.length) {
      val a0 = a(i)
      val a1 = a((i + 1) % a.length)
      for (j <- 0 until b.length) {
        val b0 = b(j)
        val b1 = b((j + 1) % b.length)
        crossPoint(a0, a1, b0, b1) match {
          case Some(p) =>
            if (p == a0 || p == a1 || p == b0 || p == b1)
              ()
            else
              return true
          case None =>
        }
      }
    }
    return false
  }

  def isInner(v: Vertex, polygon: Vector[Vertex]): Boolean = {
    var count = 0
    for (i <- 0 until polygon.length) {
      val pa = polygon(i)
      if (v == pa)
        return false
    }
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

  def createTrans2(from: Edge, to: Edge): Transform = {
    val ax = from.b - from.a
    val ay = Vertex(ax.y, -ax.x)
    val ax1 = to.b - to.a
    val ay1 = Vertex(ax1.y, -ax1.x) * (-1)
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
    println("p: " + p + " p0: " + p0)
    println("a:" + ax + ", " + ay + " (" + x + ", " + y + ")")

    val ax1 = to.b - to.a // / |ax1|
    val ay1 = Vertex(ax1.y, -ax1.x)
    println("a1:" + ax1 + ", " + ay1)

    val l2 = ax dot ax // |ax1| / |ax1|

    (ax1 * x + ay1 * y) / l2 + to.a
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
    def cross(v: Vertex): Rational = x * v.y - v.x * y

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
    {
      if (s == 0)
        throw new IllegalArgumentException
    }
    def transform(p: Vertex): Vertex = {
      val p0 = p - o
      val x = p0.dot(ax)
      val y = p0.dot(ay)
      (ax1 * x + ay1 * y) / s + o1
    }
  }

  object Transform {
    val ident = Transform(Vertex(0, 0), Vertex(1, 0), Vertex(0, 1),
      Vertex(0, 0), Vertex(1, 0), Vertex(0, 1),
      Rational(1))
  }
}

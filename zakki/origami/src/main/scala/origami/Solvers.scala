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
import scala.collection.mutable.StringBuilder

object Solver {

  val LESS = 0
  val OK = 1
  val MORE = 2

  def collectLine(p: Problem): Vector[Edge] = {
    var edges: List[Edge] = Nil
    val pe = for (p <- p.polygon; i <- 0 until p.vertices.length) {
      val pa = p.vertices(i)
      val pb = p.vertices((i + 1) % p.vertices.length)
      if (!edges.exists(_.equals(pa, pb)))
        edges ::= Edge(pa, pb)
    }
    var vertices: Set[Vertex] =
      p.polygon.flatMap(p => p.vertices).toSet
    for (e <- p.edges) {
      if (!edges.exists(_.equals(e.a, e.b)))
        edges ::= e
      vertices += e.a
      vertices += e.b
    }
    val edges2 = edges.toVector
    var es: Set[Edge] = Set()
    for (i <- 0 until edges2.length) {
      val ap1 = edges2(i)
      var ps = Set(ap1.a, ap1.b)
      for (p <- vertices) {
        val d0 = p - ap1.a
        val d1 = ap1.b - ap1.a
        if (d0.cross(d1) == 0 && d0.dot(d0) < d1.dot(d1) && d0.dot(d1) > 0) {
          ps += p
          println("division " + ap1 + " by " + p)
        }
      }

      //      for (j <- 0 until edges2.length if i != j) {
      //        val ap2 = edges(j)
      //        crossPoint(ap1, ap2) match {
      //          case Some(p) => {
      //            ps += p
      //          }
      //          case None =>
      //        }
      //      }

      val ps2 = ps.toBuffer.sortWith((a: Vertex, b: Vertex) => {
        if (a.x == b.x)
          a.y < b.y
        else
          a.x < b.x
      })
      println(ps2.length + "  " + ap1.a + " .. " + ap1.b)
      println("   " + ps)
      for (j <- 1 until ps2.length) {
        es = es + Edge(ps2(j - 1), ps2(j))
      }
      es = es + Edge(ps2.head, ps2.last)
    }
    es.toVector

    //    edges.toVector
    //    val edges2 = edges.toVector
    //    var es: Set[Edge] = Set()
    //    for (i <- 0 until edges2.length) {
    //      val ap1 = edges2(i)
    //      var ps = Set(ap1.a, ap1.b)
    //      for (j <- 0 until edges2.length if i != j) {
    //        val ap2 = edges(j)
    //        crossPoint(ap1, ap2) match {
    //          case Some(p) => {
    //            ps += p
    //          }
    //          case None =>
    //        }
    //      }
    //      val ps2 = ps.toBuffer.sortWith((a: Vertex, b: Vertex) => {
    //        if (a.x == b.x)
    //          a.y < a.y
    //        else
    //          a.x < b.x
    //      })
    //      for (j <- 1 until ps2.length) {
    //        es = es + Edge(ps2(j - 1), ps2(j))
    //      }
    //    }
    //    es.toVector
  }

  def toId(edges: Vector[Edge], vts: Vector[Vertex]): Vector[(Int, Int)] = {
    edges.map(e => (vts.indexOf(e.a), vts.indexOf(e.b)))
  }

  def isSelfCross(vertices: Vector[Vertex], vb: Vector[Int]): Boolean = {
    val n = vb.length
    for (i <- 0 until n) {
      val pa0 = vertices(vb(i))
      val pa1 = vertices(vb((i + 1) % n))
      for (j <- i + 2 until n) {
        val pb0 = vertices(vb(j))
        val pb1 = vertices(vb((j + 1) % n))
        if (pb1 != pa0) {
          val c = crossPoint(pa0, pa1, pb0, pb1)
          //println("check cross " + i + "/" + j + " " + c + " " + pa0 + ", " + pa1 + " ## " +
          //  pb0 + ", " + pb1)
          if (c.isDefined)
            return true
        }
      }
    }
    return false
  }

  def facets(edges: Vector[(Int, Int)], vertices: Vector[Vertex]) = {
    val p2eid = edges.zipWithIndex
      .flatMap { e: ((Int, Int), Int) => List((e._1._1, e._2), (e._1._2, e._2)) }
      .groupBy(t => t._1)
      .mapValues(e => e.map(_._2))
    var facets: Map[Set[Int], SEFacet] = Map()

    def isValid(p0: Int, p1: Int, p2: Int): Boolean = {
      val dir0 = vertices(p1) - vertices(p2)
      val dir = vertices(p0) - vertices(p1)
      val c = dir0.cross(dir)
      if (c == 0)
        dir0.dot(dir) > 0
      else
        c > 0
    }

    def isValidDir(vb: Vector[Int]): Boolean = {
      val n = vb.length
      var valid2 = true
      for (i <- 0 until n) {
        //println("check2 " + (i, (i + 1) % n, (i + 2) %n))
        if (!isValid(vb(i), vb((i + 1) % n), vb((i + 2) % n))) {
          //if (!isValid((i + 2) % n, (i + 1) % n, i)) {
          //println("bad facet dir " + i)
          return false
        }
      }
      return true
    }

    def needShrink(p0: Int, p1: Int, p2: Int): Boolean = {
      val dir0 = vertices(p1) - vertices(p2)
      val dir = vertices(p0) - vertices(p1)
      val c = dir0.cross(dir)
      (c == 0) && dir0.dot(dir) > 0
    }

    def next(e: Int, usedEdges: List[Int], vs: List[Int]): Unit = {
      //println("next " + e + " " + edges + "# " + vs);
      val ep = vs.head
      for (nid <- p2eid(ep) if !edges.contains(nid)) {
        //println("iter " + n + " " + e + " " + vs)
        val n = edges(nid)
        val es = nid :: usedEdges
        val (cp, np) =
          if (n._1 == ep) {
            (n._1, n._2)
          } else {
            (n._2, n._1)
          }
        val valid =
          if (vs.length >= 2) {
            //println("check1 " + (np, cp, vs(1)))
            isValid(np, cp, vs(1))
          } else {
            true
          }
        if (valid) {
          if (vs.contains(np)) {
            if (vs.length > 2 && (vs.last == np)) {
              //println("add " + n + vs)
              // if (isValid(np, cp, vs(1)))
              val vb = vs.toVector
              val n = vb.length
              var valid2 = isValidDir(vb)
              if (valid2 && isSelfCross(vertices, vb)) {
                //println("self cross")
                valid2 = false
              }
              if (valid2)
                facets += (vs.toSet -> SEFacet(vb, es.toVector))
              //else
              //println("bad facet dir " + vb)
            } else {
              //println("skip " + np + vs.last + " ## " + vs)
            }
          } else {
            next(nid, es, np :: vs)
          }
        }
      }
    }
    for (eid <- 0 until edges.length) {
      val e = edges(eid)
      //println("#start " + e + " # " + e.a);
      next(eid, List(eid), List(e._1, e._2))
      //println("#start " + e + " # " + e.b);
      next(eid, List(eid), List(e._2, e._1))
    }
    facets.values.toVector
  }

  def apply(p: Problem) = {
    val edges = Solver.collectLine(p)
    val vts = edges.flatMap { e => List(e.a, e.b) }
      .toSet.toVector
    val eids = toId(edges, vts)
    val facets = Solver.facets(eids, vts)
    println("**************************")
    println(edges)
    println(vts)
    println(eids)
    new Solver(p, vts, eids, facets, edges)
  }
}

case class SEFacet(vertices: Vector[Int], edges: Vector[Int])

case class TFacet(fid: Int, t: Transform,
                  vertices: Vector[Vertex], open: Array[Boolean]) {
}

class Solver(val problem: Problem,
             val vertices: Vector[Vertex],
             val edges: Vector[(Int, Int)],
             val facets: Vector[SEFacet],
             val _edges: Vector[Edge]) {

  val edge2facets = {
    var m: Map[Int, Vector[Int]] = Map()
    (0 until facets.length).flatMap {
      fid => facets(fid).edges.map(e => (e, fid))
    }.groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  def toFacet(f: SEFacet) = {
    Facet(f.vertices.map { vid => vertices(vid) })
  }

  def createRootNode(i: Int): Node = {
    val sf = facets(i)
    val open = (0 until sf.edges.length).map(_ => true).toArray
    val vs = sf.vertices.map(vid => vertices(vid))
    Node(Vector(TFacet(i, Transform.ident, vs, open)))
  }

  def createRoot(): Vector[Node] = {
    (for (i <- 0 until facets.length) yield {
      createRootNode(i)
    }).toVector
  }

  def hint(series: List[(Int, Int)]): List[Node] = {
    val ((_, s) :: rest) = series
    val c = createRootNode(s)
    createHintNode(rest, List(c))
  }

  def createHintNode(series: List[(Int, Int)], ns0: List[Node]): List[Node] = {
    var ns = ns0
    for (s <- series) {
      s match {
        case (-1, s) => {
          var r: List[Node] = Nil
          for (n <- ns) {
            val cs = n.expand()
              .filter(c => c.tfacets.last.fid === s)
            r :::= cs
          }
          ns = r
        }
        case (-2, s) =>
          ns = List(ns(s))
        case (t, s) => {
          var r: List[Node] = Nil
          for (n <- ns) {
            val cs = n.expand(n.tfacets(t))
              .filter(c => c.tfacets.last.fid === s)
            r :::= cs
          }
          ns = r
        }
      }
    }
    ns
  }

  def translations(t: TFacet, i: Int, s: SEFacet, eid2: Int) = {
    val org = edges(eid2) //edges(s.edges(eid2))
    val o0 = vertices(org._1)
    val o1 = vertices(org._2)
    val f = facets(t.fid)
    val e = edges(f.edges(i))
    val p0 = t.t.transform(vertices(e._1))
    val p1 = t.t.transform(vertices(e._2))
    val od = o1 - o0
    val pd = p1 - p0
    if ((od dot od) != (pd dot pd))
      println("BAD TRANSFORM " + od + "/" + pd)
    //val p0 = t.vertices(i)
    //val p1 = t.vertices((i + 1) % t.vertices.length)
    List(createTrans(Edge(o0, o1), Edge(p0, p1)),
      createTrans2(Edge(o0, o1), Edge(p0, p1)))
  }

  def dump(node: Node): String = {
    val trans = normalTrans(node.toFacets())
    if (trans.isEmpty)
      Console.out.println("no normalize transform" + node.toFacets())
	  val tvs = node.tfacets.flatMap(t => t.vertices).toSet.toVector
	  val tvsn = trans.map(tr => tvs.map(v => tr.transform(v))).getOrElse(tvs)
	  var str = tvs.length + "\n"
	  str += tvsn.map(v => v.x + "," + v.y).mkString("\n")
	  str += "\n"
	  str += node.tfacets.length + "\n"
	  for (t <- node.tfacets) {
	    str += t.vertices.length
	    str += " "
	    str += t.vertices.map(p => tvs.indexOf(p)).mkString(" ")
	    str += "\n"
	  }
	  str += tvs.map { tv =>
	    vertices(node.vmap(tv))
	  }.map(v => v.x + "," + v.y).mkString("\n")
	  str += "\n"
	  str
  }

  case class Node(val tfacets: Vector[TFacet]) {
    //    val polygons = {
    //      {
    //        for (t <- tfacets)
    //          yield facets(t.fid).vertices.map(vid => t.t.transform(vertices(vid)))
    //      }.toList
    //    }
    val (vmap, eset) = {
      var m: Map[Vertex, Int] = Map()
      var es: List[(Edge, TFacet)] = Nil
      for (
        t <- tfacets;
        f = facets(t.fid);
        i <- 0 until f.vertices.length
      ) {
        val vid = f.vertices(i)
        val p = t.vertices(i)
        val p2 = t.vertices((i + 1) % f.vertices.length)
        //if (m.contains(p) && m(p) != vid)
        //  throw new IllegalStateException()
        m += (p -> vid)
        es = (Edge(p, p2), t) :: es
      }
      val eset = es.groupBy(_._1).mapValues(_.map(_._2))
      (m, eset)
    }

    def toFacets(): Vector[Facet] = {
      tfacets.map(t => Facet(t.vertices))
    }

    def expand(): List[Node] = {
      var l: List[Node] = Nil
      tfacets.flatMap(t => expand(t)).toList
    }

    def expand(t: TFacet): List[Node] = {
      var l: List[Node] = Nil
      val f = facets(t.fid);
      for (
        i <- 0 until f.edges.size if t.open(i);
        eid = f.edges(i);
        fid2 <- edge2facets(eid)
      ) {
        //println(t.fid + "." + i + "/" + fid2)
        val f2 = facets(fid2)
        for (
          t <- translations(t, i, f2, eid)
        ) {
          val vs = f2.vertices.map(vid => t.transform(vertices(vid)))
          var valid = true
          for (j <- 0 until f2.vertices.length) {
            val tv = vs(j)
            val vid = f2.vertices(j)
            if (vmap.contains(tv) && vmap(tv) != vid) {
              //invalid
              //println("  mismatch " + vmap(tv) + "/" + vid)
              valid = false
            } else if (tfacets.exists(f => f.vertices.toSet == vs.toSet)) {
              //println("  same ")
              valid = false
            } else if (tfacets.exists(f => intersect(f.vertices, vs))) {
              //              for (f <- tfacets) {
              //                if (intersect(f.vertices, vs))
              //                	println("  intersect " + vs + "/" + f.vertices)
              //              }
              valid = false
              //            } else if (polygons.exists(poly => isInner(tv, poly))) {
              //              println("  conflict " + tv + "/" + f)
              //              valid = false
            }
          }
          //println("  " + valid + " " + t)
          if (valid) {
            val open = new Array[Boolean](f2.edges.length) //(0 until f2.edges.length).map(_ => true).toArray
            for (j <- 0 until f2.edges.length) {
              val e = Edge(vs(j), vs((j + 1) % f2.edges.length))
              open(j) = !eset.contains(e) || eset(e).length > 1
            }
            l ::= Node(tfacets :+ TFacet(fid2, t, vs, open))
          }
        }
      }
      l
    }
  }

  import Solver._

  def normalTrans(fs: Seq[Facet]): Option[Transform] = {
    val vs0 = fs.flatMap((f: Facet) => f.vertices).toSet
    val vs = vs0.toList.sortWith((a: Vertex, b: Vertex) => {
        if (a.x == b.x)
          a.y < b.y
        else
          a.x < b.x
      })
    if (vs.length < 4)
      return None
    val v0 = vs(0)
    var v1 = vs(1)
    for (i <- 2 until vs.length) {
      val v2 = vs(i)
      val d1 = v1 - v0
      val d2 = v2 - v0
      val c = d1.x * d2.y - d2.x * d1.y
      if (c < 0)
        v1 = v2
      else if (c == 0 && d1.dot(d1) < d2.dot(d2))
        v1 = v2
    }
    val du = v1 - v0
    val l = du dot du
    val dv = Vertex(du.y, -du.x)
    val r2 = vs.map(v => (v - v0) dot (v - v0)).max
    println("**** L^2:" + l + "  R^2:"+ r2)
    if (l > 1)
      return None
    if (r2 > 2)
      return None
    if (l < 1 || r2 < 2)
      return None
    //ignore scale
    val t = createTrans(Edge(v0, v1),
      Edge(Vertex(Rational(0), Rational(0)), Vertex(Rational(1), Rational(0))))
    val a = new RArea(vs.map(v => t.transform(v)))

    val t2 = t.copy(o1 = t.o1 - Vertex(a.minX, a.minY))
    return Some(t2)
  }

  def isSquareLike(vs: Seq[Vertex]): Int = {
    val v0 = vs(0)
    var v1 = vs(1)
    for (i <- 2 until vs.length) {
      val v2 = vs(i)
      val d1 = v1 - v0
      val d2 = v2 - v0
      val c = d1.x * d2.y - d2.x * d1.y
      if (c < 0)
        v1 = v2
      else if (c == 0 && d1.dot(d1) < d2.dot(d2))
        v1 = v2
    }
    val du = v1 - v0
    val l = du dot du
    val dv = Vertex(du.y, -du.x)
    val r2 = vs.map(v => (v - v0) dot (v - v0)).max
    println("****size " + l + "  "+ r2)
    if (l > 1)
      return MORE
    if (r2 > 2)
      return MORE
    if (l < 1 || r2 < 2)
      return LESS
    //ignore scale
    val t = createTrans(Edge(v0, v1),
      Edge(Vertex(Rational(0), Rational(0)), Vertex(Rational(1), Rational(0))))
    val vs1 = vs.map(v => t.transform(v))
    val a = new RArea(vs1)
    if (a.width > 1 || a.height > 1)
      return MORE
    if (a.width < 1 || a.height < 1)
      return LESS
    if (!vs1.exists(p => p.x == a.minX && p.y == a.minY)) {
      //println(vs1)
      println("no minmin " + a.minX + ", " + a.minY)
      return LESS
    }
    if (!vs1.exists(p => p.x == a.maxX && p.y == a.minY)) {
      //println(vs1)
      println("no maxmin " + a.maxX + ", " + a.minY)
      return LESS
    }
    if (!vs1.exists(p => p.x == a.minX && p.y == a.maxY)) {
      //println(vs1)
      println("no minmax " + a.minX + ", " + a.maxY)
      return LESS
    }
    if (!vs1.exists(p => p.x == a.maxX && p.y == a.maxY)) {
      //println(vs1)
      println("no maxmax " + a.maxX + ", " + a.maxY)
      return LESS
    }
    return OK
  }

  def isSquare(fs: Seq[Facet]): Int = {
    val vs0 = fs.flatMap((f: Facet) => f.vertices).toSet
    val vs = vs0.toList.sortWith((a: Vertex, b: Vertex) => {
        if (a.x == b.x)
          a.y < b.y
        else
          a.x < b.x
      })
      //.sortBy { _.x }
    if (vs.length < 4) {
      println("few vertex")
      return LESS
    }
    return isSquareLike(vs)
  }
}

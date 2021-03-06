package origami

import org.junit._
import org.junit.Assert._

import spire.implicits._
import spire.math._

import origami.math._

class SimpleTest {

  @Test
  def fold(): Unit = {
    val x = Vertex(r"4328029871649615121465353437184/8656059743299229793415925725865",
      r"-1792728671193156318471947026432/8656059743299229793415925725865")
    //println(x)
    //assertEquals(Vertex(0, 0), Fold.fold(Vertex(0, 0), Vertex(0, 1), Vertex(0, 0)))
  }

  @Test
  def testIsInner(): Unit = {
    val polygon = Vector(
      Vertex(0, 0),
      Vertex(0, 1),
      Vertex(1, 1),
      Vertex(1, 0))
    assertTrue(isInner(Vertex(Rational(1, 2), Rational(1, 2)), polygon))
    assertFalse(isInner(Vertex(0, 0), polygon))
    assertFalse(isInner(Vertex(-1, -1), polygon))
    assertFalse(isInner(Vertex(2, 2), polygon))
    assertFalse(isInner(Vertex(Rational(1, 2), -1), polygon))
    assertFalse(isInner(Vertex(Rational(1, 2), 2), polygon))
    assertFalse(isInner(Vertex(2, 0), polygon))
  }

  @Test
  def testTrans(): Unit = {
    assertEquals(Vertex(0, 0),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Vertex(0, 0)))
    assertEquals(Vertex(10, 50),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Vertex(10, 50)))
    assertEquals(Vertex(10, 0),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(10, 0), Vertex(11, 0)),
        Vertex(0, 0)))
    assertEquals(Vertex(14, 25),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(10, 20), Vertex(11, 20)),
        Vertex(4, 5)))

    assertEquals(Vertex(0, 0),
      _transform(
        Edge(Vertex(10, 50), Vertex(15, 50)),
        Edge(Vertex(0, 0), Vertex(5, 0)),
        Vertex(10, 50)))
    assertEquals(Vertex(1, 10),
      _transform(
        Edge(Vertex(10, 50), Vertex(15, 50)),
        Edge(Vertex(0, 0), Vertex(5, 0)),
        Vertex(11, 60)))

    assertEquals(Vertex(0, 0),
      _transform(
        Edge(Vertex(10, 50), Vertex(11, 49)),
        Edge(Vertex(0, 0), Vertex(1, 1)),
        Vertex(10, 50)))
    assertEquals(Vertex(1, 1),
      _transform(
        Edge(Vertex(10, 50), Vertex(11, 49)),
        Edge(Vertex(0, 0), Vertex(1, 1)),
        Vertex(11, 49)))

    assertEquals(Vertex(0, 0),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(0, 1)),
        Vertex(0, 0)))
    assertEquals(Vertex(-3, 2),
      transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(0, 1)),
        Vertex(2, 3)))

    assertEquals(Vertex(10, 5),
      transform(
        Edge(Vertex(0, 0), Vertex(2, 0)),
        Edge(Vertex(0, 0), Vertex(2, 0)),
        Vertex(10, 5)))
  }

  @Test
  def testSquare(): Unit = {
    assertTrue(Util.isSquareLike(
      List(
        Vertex(0, 0),
        Vertex(1, 0),
        Vertex(1, 1),
        Vertex(0, 1))))
    assertFalse(Util.isSquareLike(
      List(
        Vertex(0, 0),
        Vertex(2, 0),
        Vertex(1, 1),
        Vertex(0, 1))))

    assertTrue(Util.isSquareLike(
      List(
        Vertex(20, 50),
        Vertex(120, 50),
        Vertex(120, 150),
        Vertex(20, 150))))

    assertTrue(Util.isSquareLike(
      List(
        Vertex(0, 0),
        Vertex(r"3/5", r"4/5"),
        Vertex(r"-1/5", r"7/5"),
        Vertex(r"-4/5", r"3/5"))))
  }
  
  @Test
  def testCross(): Unit = {
    val c1 = crossPoint(
        Vertex(0, 5),
        Vertex(2, 5),
        Vertex(1, 0),
        Vertex(1, 20))
    assertTrue(c1.isDefined)
    assertEquals(Vertex(1, 5), c1.get)
    
    val c2 = crossPoint(
        Vertex(0, 5),
        Vertex(2, 5),
        Vertex(1, 5),
        Vertex(1, 20))
    assertTrue(c2.isDefined)
    assertEquals(Vertex(1, 5), c2.get)
    
    val c3 = crossPoint(
        Vertex(r"16/25", r"9/50"),
        Vertex(r"2/5", r"1/2"),
        Vertex(r"1", r"2/5"),
        Vertex(r"7/20", r"2/5"))
    assertTrue(c3.isDefined)
    assertEquals(Vertex(r"19/40", r"2/5"), c3.get)
  }
}

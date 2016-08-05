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
    assertTrue(Util.isInner(Vertex(Rational(1, 2), Rational(1, 2)), polygon))
    assertTrue(Util.isInner(Vertex(0, 0), polygon))
    assertFalse(Util.isInner(Vertex(-1, -1), polygon))
    assertFalse(Util.isInner(Vertex(2, 2), polygon))
    assertFalse(Util.isInner(Vertex(Rational(1, 2), -1), polygon))
    assertFalse(Util.isInner(Vertex(Rational(1, 2), 2), polygon))
    assertFalse(Util.isInner(Vertex(2, 0), polygon))
  }

  @Test
  def testTrans(): Unit = {
    assertEquals(Vertex(0, 0),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Vertex(0, 0)))
    assertEquals(Vertex(10, 50),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Vertex(10, 50)))
    assertEquals(Vertex(10, 0),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(10, 0), Vertex(11, 0)),
        Vertex(0, 0)))
    assertEquals(Vertex(14, 25),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(10, 20), Vertex(11, 20)),
        Vertex(4, 5)))

    assertEquals(Vertex(0, 0),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(0, 1)),
        Vertex(0, 0)))
    assertEquals(Vertex(-3, 2),
      Util.transform(
        Edge(Vertex(0, 0), Vertex(1, 0)),
        Edge(Vertex(0, 0), Vertex(0, 1)),
        Vertex(2, 3)))

    assertEquals(Vertex(10, 5),
      Util.transform(
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
}

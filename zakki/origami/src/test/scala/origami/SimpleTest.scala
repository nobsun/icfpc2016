package origami

import org.junit._
import org.junit.Assert._

import spire.implicits._
import spire.math._
import sun.security.provider.certpath.Vertex

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
        Vertex(1, 0)
    		)
    assertTrue(Util.isInner(Vertex(Rational(1, 2), Rational(1, 2)), polygon))
    assertTrue(Util.isInner(Vertex(0, 0), polygon))
    assertFalse(Util.isInner(Vertex(-1, -1), polygon))
    assertFalse(Util.isInner(Vertex(2, 2), polygon))
    assertFalse(Util.isInner(Vertex(Rational(1, 2), -1), polygon))
    assertFalse(Util.isInner(Vertex(Rational(1, 2), 2), polygon))
    assertFalse(Util.isInner(Vertex(2, 0), polygon))
  }
}

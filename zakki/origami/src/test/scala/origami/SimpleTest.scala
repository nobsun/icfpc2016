package origami

import org.junit._
import org.junit.Assert._

import spire.implicits._
import spire.math._

class SimpleTest {

  @Test
  def fold(): Unit = {
    val x = Vertex(r"4328029871649615121465353437184/8656059743299229793415925725865",
        r"-1792728671193156318471947026432/8656059743299229793415925725865")
    println(x)
    assertEquals(Vertex(0, 0), Fold.fold(Vertex(0, 0), Vertex(0, 1), Vertex(0, 0)))
  }

}

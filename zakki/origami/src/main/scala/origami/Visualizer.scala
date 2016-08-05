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

object Visualizer {
  def saveImage(p: Problem, filename: String) = {
    val img = drawImage(p)
    ImageIO.write(img, "png", new File(filename))
  }

  def drawImage(p: Problem) = {
    val size = 500
    val pad = 40
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

    def getx(x: Rational) = ((x - minX) / w).toDouble * (size - pad * 2) + pad
    def gety(y: Rational) = ((maxY - y) / h).toDouble * (size - pad * 2) + pad

    for (poly <- p.polygon) {
      var start = true
      for (v <- poly.vertices) {
        val x = getx(v.x)
        val y = gety(v.y)
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

    for (e <- p.edges) {
      val x1 = getx(e.a.x)
      val y1 = gety(e.a.y)
      val x2 = getx(e.b.x)
      val y2 = gety(e.b.y)
      g.setColor(Color.BLACK)
      g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)

      g.setColor(Color.CYAN.darker())
      g.drawString(e.a.toShortString(), x1.toInt - 10, y1.toInt + 5)
      g.drawString(e.b.toShortString(), x2.toInt - 10, y2.toInt + 5)
    }

    for (poly <- p.polygon; v <- poly.vertices) {
      val x = getx(v.x)
      val y = gety(v.y)
      g.setColor(Color.GREEN.darker())
      g.drawString(v.toShortString(), x.toInt - 10, y.toInt + 5)
    }

    g.dispose()
    img
  }
}


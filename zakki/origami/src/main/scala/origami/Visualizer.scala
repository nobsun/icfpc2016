package origami

import spire.implicits._
import spire.math._
import scala.collection.immutable.Vector
import java.awt.image.BufferedImage
import java.awt.geom._
import java.awt.Color
import java.awt.Graphics2D
import java.io.Writer
import java.io.OutputStreamWriter
import javax.imageio.ImageIO
import java.io.File
import origami.math._

import org.apache.batik.svggen.SVGGraphics2D
import org.apache.batik.dom.GenericDOMImplementation

import org.w3c.dom.Document
import org.w3c.dom.DOMImplementation
import java.io.FileOutputStream

object Visualizer {
  def saveImage(p: Problem, f: File) = {
    if (!f.getParentFile().exists())
      f.getParentFile().mkdirs()
    val size = 500
    val pad = 40
    val img = new BufferedImage(size, size, BufferedImage.TYPE_3BYTE_BGR)
    val g = img.getGraphics().asInstanceOf[Graphics2D]
    drawImage(p, g, size, pad)
    ImageIO.write(img, "png", f)
  }

  def saveSvg(p: Problem, f: File) = {
    if (!f.getParentFile().exists())
      f.getParentFile().mkdirs()
    val size = 500
    val pad = 40
    val img = new BufferedImage(size, size, BufferedImage.TYPE_3BYTE_BGR)

    val domImpl = GenericDOMImplementation.getDOMImplementation();
    val svgNS = "http://www.w3.org/2000/svg";
    val document = domImpl.createDocument(svgNS, "svg", null);

    // Create an instance of the SVG Generator.
    val g = new SVGGraphics2D(document);
    drawImage(p, g, size, pad)
    val out = new OutputStreamWriter(new FileOutputStream(f), "UTF-8")
    g.stream(out, true)//use css
  }

  def saveImage(p: Solution, f: File) = {
    if (!f.getParentFile().exists())
      f.getParentFile().mkdirs()
    val img = drawImage(p)
    ImageIO.write(img, "png", f)
  }

  def drawImage(p: Problem, g: Graphics2D, size: Int, pad: Int) = {
    val area = Util.areaOf(p)
    val cx = (area.minX + area.maxX) / 2
    val cy = (area.minY + area.maxY) / 2
    area.update(Vertex(cx + r"1/2", cy + r"1/2"))
    area.update(Vertex(cx - r"1/2", cy - r"1/2"))
    val minX = area.minX
    val minY = area.minY
    val maxX = area.maxX
    val maxY = area.maxY
    val w_ = maxX - minX
    val h_ = maxY - minY
    val l = max(w_, h_)
    val w = l
    val h = l
    val path = new GeneralPath(Path2D.WIND_EVEN_ODD)

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size, size)

    def getx(x: Rational) = ((x - minX) / w * (size - pad * 2)).toInt + pad
    def gety(y: Rational) = ((maxY - y) / h * (size - pad * 2)).toInt + pad

    g.setColor(Color.RED)
    g.drawLine(0, 0, size, size)

    g.setColor(Color.GREEN)
    g.fillRect(getx(cx - r"1/2"), gety(cy + r"1/2"),
        (1 / w * (size - pad * 2)).toInt,
        (1 / h * (size - pad * 2)).toInt)

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

    g.setColor(Color.BLACK)
    for (poly <- p.polygon) {
      for ((v, i) <- poly.vertices.zipWithIndex) {
        val x = getx(v.x)
        val y = gety(v.y) + pad / 2
        g.drawString(('a' + i).toChar.toString, x, y)
      }
    }

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
  }

  def drawImage(p: Solution) = {
    val size = 500
    val pad = 40
    val srcArea = new RArea(p.source)
    val dstArea = new RArea(p.destination)
    val l = max(max(srcArea.width, srcArea.height), max(dstArea.width, dstArea.height))
    val path = new GeneralPath(Path2D.WIND_EVEN_ODD)

    val img = new BufferedImage(size * 2, size, BufferedImage.TYPE_3BYTE_BGR)
    val g = img.getGraphics().asInstanceOf[Graphics2D]
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size * 2, size)

    g.setColor(Color.RED)
    g.drawLine(0, 0, size, size)

    def getsx(x: Rational) = ((x - srcArea.minX) / l * (size - pad * 2)).toInt + pad
    def getsy(y: Rational) = ((srcArea.maxY - y) / l * (size - pad * 2)).toInt + pad

    def getdx(x: Rational) = size + ((x - dstArea.minX) / l * (size - pad * 2)).toInt + pad
    def getdy(y: Rational) = ((dstArea.maxY - y) / l * (size - pad * 2)).toInt + pad

    for (f <- p.facets) {
      var start = true
      for (v <- f.vertices.map(i => p.source(i))) {
        val x = getsx(v.x)
        val y = getsy(v.y)
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
    g.setColor(Color.BLACK)
    g.draw(path)

    for (f <- p.facets) {
      var start = true
      for (v <- f.vertices.map(i => p.destination(i))) {
        val x = getdx(v.x)
        val y = getdy(v.y)
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
    g.setColor(Color.BLACK)
    g.draw(path)

    //    for (poly <- p.polygon; v <- poly.vertices) {
    //      val x = getx(v.x)
    //      val y = gety(v.y)
    //      g.setColor(Color.GREEN.darker())
    //      g.drawString(v.toShortString(), x.toInt - 10, y.toInt + 5)
    //    }

    g.dispose()
    img
  }
}


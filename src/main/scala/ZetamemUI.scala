import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import java.util.{Timer, TimerTask}

import javax.imageio.ImageIO
import net.team2xh.onions.Themes
import net.team2xh.onions.Themes.ColorScheme
import net.team2xh.onions.components.widgets.{BitMap, Label}
import net.team2xh.onions.components.{Frame, FramePanel, Widget}
import net.team2xh.scurses.Scurses

import scala.util.Random
case class Grid(parent: FramePanel, initialValues: List[List[Int]] = Nil )(implicit screen: Scurses) extends Widget(parent) {

  private val maxColumnsPerStrip = screen.size._1 / 6.25
  private var columnsByStrip = Map.empty[Int, List[List[Int]]]
  private var currentStrip = 0

  private val CEIL = "╔═══╗"
  private val FLOOR= "╚═══╝"
  private val WALL = "║"
  private val MEZZANINE = "╠═══╣"

  def addColumn(column: List[Int]): Unit = {
    if (columnsByStrip.getOrElse(currentStrip, List.empty).size < maxColumnsPerStrip) {
      columnsByStrip = columnsByStrip.updated(currentStrip, columnsByStrip.getOrElse(currentStrip, List.empty) :+ column)
    } else {
      currentStrip = currentStrip + 1
      columnsByStrip = columnsByStrip + (currentStrip -> List(column))
    }
    needsRedraw = true
  }

  override def focusable: Boolean = true

  private def drawComponent(x: Int, y: Int)(block: String): Unit = screen.put(x, y, block)
  private def drawFloor(x: Int, y: Int): Unit = drawComponent(x,y)(FLOOR)
  private def drawCeil(x: Int, y: Int): Unit = drawComponent(x,y)(CEIL)
  private def drawWall(x: Int, y: Int): Unit = drawComponent(x,y)(WALL)
  private def drawMezzanine(x: Int, y: Int): Unit = drawComponent(x,y)(MEZZANINE)

  def drawMezzanines(quantity: Int, startingAtX: Int, startingAtY: Int): Unit = (1 to quantity) foreach(index => {
    drawMezzanine(startingAtX, startingAtY + (2 * index))
  })

  def drawLeftWalls(quantity: Int, startingAtX: Int, startingAtY: Int) = 0 until quantity foreach(index => {
    drawWall(startingAtX, startingAtY + ((index * 2) + 1))
  })

  def drawRightWalls(quantity: Int, startingAtX: Int, startingAtY: Int) = 0 until quantity foreach(index => {
    drawWall(startingAtX + 4, startingAtY + ((index * 2) + 1))
  })

  def drawColumn(column: List[Int], x: Int, y: Int): Unit =  {
    //draw empty columns
    column.zipWithIndex.map { case(value, index) => value -> index } match {
      case _ :: Nil =>
        drawCeil(x, y)
        drawWall(x, y + 1)
        drawWall(x + 4, y + 1)
        drawFloor(x, y + 2)
      case list =>
        drawCeil(x, y)
        drawMezzanines(list.size - 1 , x, y)
        drawLeftWalls(list.size, x, y)
        drawRightWalls(list.size, x, y)
        drawFloor(x, y + (list.size * 2))
    }

    //fill them
    column.zipWithIndex.foreach{
      case(value, index) => screen.put(x + 2, y + (index * 2) + 1 , value.toString)
    }
  }

  override def redraw(focus: Boolean, theme: ColorScheme): Unit = {
    columnsByStrip.foreach{
      case (strip, columns) => columns.zipWithIndex.foreach{
        case (column, index) => drawColumn(column, index * 6, strip * 10)
      }
    }
  }

  override def handleKeypress(keypress: Int): Unit = {}

  override def innerHeight: Int = parent.height
}

object ZetamemUI extends App {

  val clockTimer = new Timer()
  val r = Random

  Scurses { implicit screen =>
    val frame = Frame(Some("Page Replacement Simulation by Zeta"),debug = true, theme = Themes.default)
    val colA = frame.panel
    val colB = colA.splitRight

    BitMap(colB, "/logo.png", relative = true)
    val grid = Grid(colA)

    clockTimer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = {
        grid.addColumn(List(1,2,3,4))
        frame.redraw()
      }
    }, 1000, 500)
    frame.show()
  }

  clockTimer.cancel()
}


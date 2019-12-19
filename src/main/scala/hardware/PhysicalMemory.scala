package hardware
import Types.{Frame, Page}
import software.PageTable

import scala.language.postfixOps

object PhysicalMemory {
  def apply(numberOfFrames: Int): PhysicalMemory = PhysicalMemory.apply(
    (0 until numberOfFrames).map {
      _ -> None
    } toMap
  )
}

case class PhysicalMemory(frames: Map[Frame, Option[Page]] = Map.empty, pageTable: PageTable = PageTable()) {

  def size = frames.size

  def getPage(frame: Frame) = frames.get(frame).flatten

  def allocatedPages = frames.values.flatten.toList // TODO: THIS SHOULD BE THE PAGE TABLE
  def +(frame: Frame, page: Page): PhysicalMemory = {
    println(s"Added Paage $page on Frame $frame")
    this.copy(
      frames = frames + (frame -> Some(page)),
      pageTable = pageTable + (page -> frame)
    )
  }

  def +(frameAndPages: (Frame, Page)*): PhysicalMemory = frameAndPages.foldLeft(this){ case (memory,(frame, page)) => memory + (frame, page)}
}

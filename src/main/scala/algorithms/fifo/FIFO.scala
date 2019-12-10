package algorithms.fifo
import Types.{Frame, Page}
import algorithms.PageReplacementAlgorithm
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

import scala.language.postfixOps

case class FIFO() extends PageReplacementAlgorithm {

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = {
    val tlb = TranslationLookAsideBuffer()
    val pageTable = PageTable()
    def handle(pageReferenceChain: List[Page], physicalMemory: PhysicalMemory, frames: Iterator[Frame]): PhysicalMemory = pageReferenceChain match {
      case demandedPage :: nextPages if isPageCurrentlyAllocated(tlb, pageTable, demandedPage) =>
        handle(nextPages, physicalMemory, frames)
      case demandedPage :: nextPages =>
        val frame = frames.next()
//        tlb remove (physicalMemory.getPage(frame).get)
        tlb + (demandedPage -> frame)
        physicalMemory.getPage(frame).map(page => tlb remove (page))
        handle(nextPages, physicalMemory + (frame, demandedPage), frames)
      case Nil =>
        physicalMemory
    }

    val frames = Iterator.continually(physicalMemory.frames.keySet).flatten
    handle(pageReferenceChain, physicalMemory, frames)
  }
}

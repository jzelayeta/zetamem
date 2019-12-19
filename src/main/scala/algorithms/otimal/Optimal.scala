package algorithms.otimal

import Types.Page
import algorithms.PageReplacementAlgorithm
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

case class Optimal() extends PageReplacementAlgorithm {
  implicit val tlb = TranslationLookAsideBuffer()
  val pageTable = PageTable()
  override def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = pageReferenceChain match {
    case demandedPage :: nextPages if isPageCurrentlyAllocated(demandedPage)(tlb, physicalMemory) =>
      handle(physicalMemory, nextPages)
    case demandedPage :: nextPages if memoryHasFreeFrames(physicalMemory) =>
      val frame = tlb.size
      tlb + (demandedPage -> frame)
      handle(physicalMemory + (frame -> demandedPage), nextPages)
    case demandedPage :: nextPages =>
      nextPages.view.reverse.map(page => tlb.get(page).map(frame => page -> frame)).find(_.isDefined).flatten match {
        case Some((page, frame)) => //TLB hit
          tlb remove page
          tlb + (demandedPage -> frame)
          handle(physicalMemory + (frame -> demandedPage), nextPages)
        case _  => // TLB miss, page is not allocated
          val (page, frame) = tlb.head
          tlb remove page
          tlb + (frame, demandedPage)
          handle(physicalMemory + (frame -> demandedPage), nextPages)
      }
    case Nil  => physicalMemory
  }
}

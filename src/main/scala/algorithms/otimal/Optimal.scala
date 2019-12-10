package algorithms.otimal

import Types.Page
import algorithms.PageReplacementAlgorithm
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

case class Optimal() extends PageReplacementAlgorithm {
  val tlb = TranslationLookAsideBuffer()
  val pageTable = PageTable()
  override def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = pageReferenceChain match {
    case demandedPage :: nextPages if isPageCurrentlyAllocated(tlb, pageTable, demandedPage) =>
      tlb update demandedPage
      handle(physicalMemory, nextPages)
    case demandedPage :: nextPages if memoryHasFreeFrames(physicalMemory) =>
      tlb + (demandedPage -> tlb.size)
      handle(physicalMemory + (tlb(demandedPage) -> demandedPage), nextPages)
    case demandedPage :: nextPages =>
      nextPages.view.reverse.map(page => tlb.get(page).map(_ => page)).find(_.isDefined).flatten match {
        case Some(page) =>
          val victimFrame = tlb(page)
          tlb remove page
          tlb + (demandedPage -> victimFrame)
          handle(physicalMemory + (victimFrame -> demandedPage), nextPages)
        case _  =>
          val (page, frame) = tlb.head
          tlb remove page
          tlb + (frame, demandedPage)
          handle(physicalMemory + (frame -> demandedPage), nextPages)
      }
    case Nil  => physicalMemory
  }
}

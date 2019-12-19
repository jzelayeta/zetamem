package algorithms.lru
import Types.{Frame, Page}
import algorithms.PageReplacementAlgorithm
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

import scala.language.postfixOps

case class LRU() extends PageReplacementAlgorithm {

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = {

    val lruQueue = LruQueue(physicalMemory.frames.size)
    val tlb: TranslationLookAsideBuffer = TranslationLookAsideBuffer()
    val pageTable: PageTable = PageTable()

    def handle(pageReferenceChain: List[Page], physicalMemory: PhysicalMemory, frames: Iterator[Frame], lastRecentlyUsed: LruQueue): PhysicalMemory = pageReferenceChain match {
      case demandedPage :: nextPages if  isPageCurrentlyAllocated(demandedPage)(tlb, physicalMemory) =>
        lruQueue + (demandedPage -> tlb(demandedPage))
        handle(nextPages, physicalMemory, frames, lastRecentlyUsed)
      case demandedPage :: nextPages if memoryHasFreeFrames(physicalMemory) =>
        val frame = frames.next()
        lruQueue + (demandedPage -> frame)
        tlb + (demandedPage -> frame)
        handle(nextPages, physicalMemory + (frame -> demandedPage), frames, lruQueue)
      case demandedPage :: nextPages =>
        lruQueue lastRecentlyUsed match {
          case Some((page, frame)) =>
            tlb remove page
            tlb + (demandedPage, frame)
            //            lruQueue - (page, frame)
            lruQueue + (demandedPage, frame)
            handle(nextPages, physicalMemory + (frame, demandedPage), frames, lastRecentlyUsed)
        }
      case Nil => physicalMemory
    }

    val frames = Iterator.continually(physicalMemory.frames.keySet).flatten
    handle(pageReferenceChain, physicalMemory, frames, lruQueue)
  }
}

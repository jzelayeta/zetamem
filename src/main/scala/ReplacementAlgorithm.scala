import Types.{Frame, Page}

import scala.collection.immutable.{HashMap, Queue}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object PhysicalMemory {
  def apply(numberOfFrames: Int): PhysicalMemory = PhysicalMemory.apply(
    (0 until numberOfFrames).map {
      _ -> None
    } toMap
  )
}

case class MemoryManagementUnit(physicalMemory: PhysicalMemory, TLB: TranslationLookAsideBuffer) {
  def doesTLBContainsPage(page: Page) = TLB contains page
  def memoryHasFreeFrames() = physicalMemory.frames.values.toList.contains(None)
  def lookFrameInTLB(page: Page) = TLB.get(page)
}

object TranslationLookAsideBuffer {
  def apply(): TranslationLookAsideBuffer = new TranslationLookAsideBuffer()
}
case class TranslationLookAsideBuffer(pagesAndFrames: (Page, Frame)*) extends scala.collection.mutable.HashMap[Page, Frame] {
  this ++= pagesAndFrames

  def +(page: Page, frame: Frame) = this addOne (page -> frame)

  def +(pageAndFrame: (Page, Frame)) = this addOne pageAndFrame
}

case class PhysicalMemory(frames: Map[Frame, Option[Page]] = Map.empty) {
  def size = frames.size
  def getFrame(frame: Frame) = frames.get(frame)
  def allocatedPages  = frames.values.flatten.toList // TODO: THIS SHOULD BE THE PAGE TABLE
  def + (frame: Frame, page: Page): PhysicalMemory = {
    println(s"Added Paage $page on Frame $frame")
    this.copy(
      frames + (frame -> Some(page))
    )
  }

  def +(frameAndPage: (Frame, Page)): PhysicalMemory = this + (frameAndPage._1, frameAndPage._2)
}

case class ReplacementAlgorithm() {
  def memoryHasFreeFrames(physicalMemory: PhysicalMemory) = physicalMemory.frames.values.toList.contains(None)

  def isFrameFree(physicalMemory: PhysicalMemory, frame: Frame): Boolean = (physicalMemory getFrame frame) isEmpty

  def isPageCurrentlyAllocated(physicalMemory: PhysicalMemory, page: Page): Boolean = physicalMemory.allocatedPages contains page //TODO: THIS BEHAVIOUR IS NOT RESPONSABILITY OF THIS OBJECT, INSTEAD IS RESPONSABILITY OF THE MMU

  def replacePage(physicalMemory: PhysicalMemory, frame: Frame, page: Page) = physicalMemory + (frame, page)

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = {
    def handle(pageReferenceChain: List[Page], physicalMemory: PhysicalMemory, frames: Iterator[Frame]): PhysicalMemory = pageReferenceChain match {
      case demandedPage :: nextPages if isPageCurrentlyAllocated(physicalMemory, demandedPage) =>
        handle(nextPages, physicalMemory, frames)
      case demandedPage :: nextPages =>
        handle(nextPages, physicalMemory + (frames.next(), demandedPage), frames)
      case Nil =>
        physicalMemory
    }

    val frames = Iterator.continually(physicalMemory.frames.keySet).flatten
    handle(pageReferenceChain, physicalMemory, frames)
  }
}


//TODO: INCLUDE TIME REFERENCE
case class LRUQueue(maxSize: Int) {
  val elements = ListBuffer[(Page, Frame)]()
  val allocated = mutable.HashMap[(Page, Frame), Int]()  // A map that stores page and frame with index in list `elements`

  def +(e: (Page, Frame)) = allocated.get(e).map{ index =>
    elements.remove(index)
    elements.addOne(e)
    elements.zipWithIndex.foldLeft(allocated){(map, e) => map.addOne(e)}
    println(elements)
  }.getOrElse({
    if(elements.size == maxSize ) {
      lastRecentlyUsed map(-)
      elements.zipWithIndex.foldLeft(allocated){(map, e) => map.addOne(e)}
    }
    allocated.addOne(e -> elements.size)
    elements.addOne(e)
  })

  def -(e: (Page, Frame)) = allocated.remove(e).map{elements.remove}

  def +(elems: Iterable[(Page, Frame)]) = elements prependedAll elems

  def headOption = elements.headOption

  def lastRecentlyUsed = if (elements.size > maxSize) Some(elements.reverse(maxSize - 1)) else elements.headOption
}

case class LruReplacementAlgorithm() {
  def memoryHasFreeFrames(physicalMemory: PhysicalMemory) = physicalMemory.frames.values.toList.contains(None)

  def isFrameFree(physicalMemory: PhysicalMemory, frame: Frame): Boolean = (physicalMemory getFrame frame) isEmpty

  def isPageCurrentlyAllocated(tlb: TranslationLookAsideBuffer, page: Page): Boolean = tlb.get(page).isDefined //TODO: THIS BEHAVIOUR IS NOT RESPONSABILITY OF THIS OBJECT, INSTEAD IS RESPONSABILITY OF THE MMU

  def replacePage(physicalMemory: PhysicalMemory, frame: Frame, page: Page) = physicalMemory + (frame, page)

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory = {

    val lruQueue = LRUQueue(physicalMemory.frames.size)
    val tlb = TranslationLookAsideBuffer()

    def handle(pageReferenceChain: List[Page], physicalMemory: PhysicalMemory, frames: Iterator[Frame], lastRecentlyUsed: LRUQueue): PhysicalMemory = pageReferenceChain match {
      case demandedPage :: nextPages if  isPageCurrentlyAllocated(tlb, demandedPage) =>
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

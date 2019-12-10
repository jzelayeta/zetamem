package algorithms

import Types.Page
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

trait PageReplacementAlgorithm {

  def memoryHasFreeFrames(physicalMemory: PhysicalMemory) = physicalMemory.frames.values.toList.contains(None)

  def isPageCurrentlyAllocated(tlb: TranslationLookAsideBuffer, pageTable: PageTable, page: Page): Boolean = tlb.get(page).orElse(pageTable.get(page)).isDefined //TODO: THIS BEHAVIOUR IS NOT RESPONSABILITY OF THIS OBJECT, INSTEAD IS RESPONSABILITY OF THE MMU

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory

}

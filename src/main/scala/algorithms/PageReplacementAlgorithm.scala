package algorithms

import Types.Page
import hardware.{PhysicalMemory, TranslationLookAsideBuffer}
import software.PageTable

trait PageReplacementAlgorithm {

  def memoryHasFreeFrames(physicalMemory: PhysicalMemory) = physicalMemory.frames.values.toList.contains(None)

  def isPageCurrentlyAllocated(page: Page)(tlb: TranslationLookAsideBuffer, physicalMemory: PhysicalMemory): Boolean = tlb.get(page).orElse(None).isDefined
  /*
  TODO: THIS BEHAVIOUR IS NOT RESPONSABILITY OF THIS OBJECT, INSTEAD IS RESPONSABILITY OF THE MMU
  TODO: in orElse body => retrieve page table from physicalMemory and , look if page is allocated.
  */

  def handle(physicalMemory: PhysicalMemory, pageReferenceChain: List[Page]): PhysicalMemory

}

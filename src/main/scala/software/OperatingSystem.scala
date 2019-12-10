package software

import algorithms.PageReplacementAlgorithm
import hardware.{MemoryManagementUnit, PhysicalMemory, TranslationLookAsideBuffer}

case class OperatingSystem(physicalMemory: PhysicalMemory, TLB: TranslationLookAsideBuffer, MMU: MemoryManagementUnit,
                           pageTable: PageTable, pageReplacementAlgorithm: PageReplacementAlgorithm) {

//  def loadProcess(process: Process) = pageReplacementAlgorithm.handle(process.pageReferences)
}
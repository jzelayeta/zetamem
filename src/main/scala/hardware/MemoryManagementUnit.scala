package hardware

import Types.Page

case class MemoryManagementUnit(physicalMemory: PhysicalMemory, TLB: TranslationLookAsideBuffer) {
  def doesTLBContainsPage(page: Page) = TLB contains page
  def memoryHasFreeFrames() = physicalMemory.frames.values.toList.contains(None)
  def lookFrameInTLB(page: Page) = TLB.get(page)
}

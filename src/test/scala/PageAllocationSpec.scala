import algorithms.fifo.FIFO
import algorithms.lru.{LRU, LruQueue}
import algorithms.otimal.Optimal
import hardware.{MemoryManagementUnit, PhysicalMemory, TranslationLookAsideBuffer}
import org.scalatest.{Matchers, WordSpecLike}

class PageAllocationSpec extends WordSpecLike with Matchers {

  "TLB" should {
    "be empty when created" in {
      val TLB = TranslationLookAsideBuffer()
      TLB shouldBe empty
    }

    "add entries to the table" in {
      val TLB = TranslationLookAsideBuffer()
      TLB += 3 -> 2
      TLB(3) shouldBe 2
    }

    "remove entries to the table" in {
      val TLB = TranslationLookAsideBuffer()
      TLB += 3 -> 2
      TLB remove 3
      TLB shouldBe empty
    }
  }

  "MMU" should {
    "check if TLB contains page" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val TLB = TranslationLookAsideBuffer()
      TLB += 3 -> 2
      val MMU = MemoryManagementUnit(physicalMemory, TLB)
      MMU.doesTLBContainsPage(3) shouldBe true
    }

    "get frame for a page in TLB" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val TLB = TranslationLookAsideBuffer()
      TLB += 3 -> 2
      val MMU = MemoryManagementUnit(physicalMemory, TLB)
      MMU.lookFrameInTLB(page = 3) shouldBe Some(2)
    }

    "should check in Page Table if page was not present in TLB" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)

    }
  }

  "LRU Cache" should {
    "receive elements" in {
      val lruQueue = LruQueue(5)

      lruQueue + (1 -> 0)

      lruQueue.headOption shouldBe Some(1 -> 0)
      lruQueue.lastRecentlyUsed shouldBe Some(1 -> 0)
    }

    "add and preseve order" in {
      val lruQueue = LruQueue(3)
      lruQueue + (1 -> 0)
      lruQueue + (2 -> 1)
      lruQueue + (3 -> 2)

      lruQueue.lastRecentlyUsed shouldBe Some(1 -> 0)
    }
  }

  "Physical Memory" should {
    "have a fixed number of frames" in {
      val physicalMemory = PhysicalMemory(3)
      physicalMemory.size shouldBe 3
    }

    "be initialized with all empty frames" in {
      val physicalMemory = PhysicalMemory(3)
      physicalMemory.frames shouldBe Map(0 -> None, 1 -> None, 2 -> None)
    }
  }
  "Page allocation" should {

    //THIS IS MMU BEHAVIOUR
    "check if physical memory has free frames" in {
      val physicalMemory = PhysicalMemory(3)
      val fifo = FIFO()
      fifo.memoryHasFreeFrames(physicalMemory) shouldBe true
    }

    //THIS IS MMU BEHAVIOUR
    "check if physical memory contains a page" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val tlb = TranslationLookAsideBuffer()
      val mmu = MemoryManagementUnit(physicalMemory, tlb)

    }

    "handle a page reference chain with FIFO Replacement Algorithm with exactly the same number of avaiable frames" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val fifo = FIFO()
      val pageReferenceChain = List(2,3,5)
      val memoryState = fifo.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(2), 1 -> Some(3), 2 -> Some(5))
    }

    "handle a larger page chain refererence with FIFO Replacement Algorithm" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val fifo = FIFO()
      val pageReferenceChain = List(2,3,2,1,5,2,4,5,3,2,5,2)
      val memoryState = fifo.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(3), 1 -> Some(5), 2 -> Some(2))
    }

    "handle page references with LRU" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val lru = LRU()
      val pageReferenceChain = List(2,3,5,2,3,1,3,5,2,4)
      val memoryState = lru.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(5), 1 -> Some(4), 2 -> Some(2))
    }

    "handle other LRU" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val lru = LRU()
      val pageReferenceChain = List(2,3,2,1,5,2,4,5,3,2,5,2)
      val memoryState = lru.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(3), 1 -> Some(5), 2 -> Some(2))
    }

    "handle optimum" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val optimal = Optimal()
      val pageReferenceChain = List(2,3,2)
      val memoryState = optimal.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(2), 1 -> Some(3), 2 -> None)
    }

    "handle optimum1" in {
      val physicalMemory = PhysicalMemory(numberOfFrames = 3)
      val optimal = Optimal()
      val pageReferenceChain = List(2,3,2,1,5)
      val memoryState = optimal.handle(physicalMemory, pageReferenceChain)
      memoryState.frames shouldBe Map(0 -> Some(5), 1 -> Some(3), 2 -> Some(1))
    }
  }
}

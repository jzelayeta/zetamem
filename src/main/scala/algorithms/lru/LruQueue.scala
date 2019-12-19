package algorithms.lru
import Types.{Frame, Page}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class LruQueue(maxSize: Int) {
  private val elements = ListBuffer[(Page, Frame)]()
  private val allocated = mutable.HashMap[(Page, Frame), Int]()  // A map that stores page and frame with index in list `elements`

  def +(e: (Page, Frame)) = allocated.get(e).map{ index =>
    elements.remove(index)
    elements.+=(e)
    elements.zipWithIndex.foldLeft(allocated){(map, e) => map.+=(e)}
    println(elements)
  }.getOrElse({
    if(elements.size == maxSize ) {
      lastRecentlyUsed map(-)
      elements.zipWithIndex.foldLeft(allocated){(map, e) => map.+=(e)}
    }
    allocated.+=(e -> elements.size)
    elements.+=(e)
  })

  def -(e: (Page, Frame)): Option[(Page, Frame)] = allocated.remove(e).map{elements.remove}

  def headOption: Option[(Page, Frame)] = elements.headOption

  def lastRecentlyUsed: Option[(Page, Frame)] = if (elements.size > maxSize) Some(elements.reverse(maxSize - 1)) else elements.headOption
}

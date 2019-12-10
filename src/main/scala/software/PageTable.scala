package software

import Types.{Frame, Page}

object PageTable {
  def apply(): PageTable = new PageTable()
}
case class PageTable (pagesAndFrames: (Page, Frame)*) extends scala.collection.mutable.HashMap[Page, Frame] {
  this ++= pagesAndFrames

  def +(page: Page, frame: Frame): PageTable = this addOne (page -> frame)

  def +(pageAndFrame: (Page, Frame)): PageTable = this addOne pageAndFrame

  def pagesAllocated(): collection.Set[Page] = this.keySet
}
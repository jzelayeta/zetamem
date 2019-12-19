package hardware

import Types.{Frame, Page}

object TranslationLookAsideBuffer {
  def apply(): TranslationLookAsideBuffer = new TranslationLookAsideBuffer()
}
case class TranslationLookAsideBuffer(pagesAndFrames: (Page, Frame)*) extends scala.collection.mutable.LinkedHashMap[Page, Frame] {
  this ++= pagesAndFrames

  def +(page: Page, frame: Frame): TranslationLookAsideBuffer = this += (page -> frame)

  def +(pageAndFrame: (Page, Frame)): TranslationLookAsideBuffer = this += pageAndFrame
}

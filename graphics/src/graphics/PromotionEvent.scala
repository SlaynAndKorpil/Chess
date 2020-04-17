package graphics

import chess.graphics.PromoOptionButton

/**
  * [Description]
  *
  * @author Felix Lehner
  * @version
  */
case class PromotionEvent(override val source: PromoOptionButton) extends swing.event.ActionEvent(source)

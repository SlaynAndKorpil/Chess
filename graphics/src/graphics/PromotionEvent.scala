package graphics

case class PromotionEvent(override val source: PromoOptionButton) extends swing.event.ActionEvent(source)

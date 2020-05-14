package graphics

object Main extends App {
  val window = new CWindow(1200, 1200, args.mkString(" "))
  window.display()
}

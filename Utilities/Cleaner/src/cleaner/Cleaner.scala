package cleaner

object Cleaner extends App {
  val obj = new Clean_src(args(0), args(1))
  obj clean
}
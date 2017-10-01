object Countdown {

  @annotation.tailrec
  def countdown(i: Int): Unit = {
    println(i)
    if (i <= 0) return
    else countdown(i - 1)
  }

  def main(args: Array[String]): Unit = {
    countdown(5)
  }

}
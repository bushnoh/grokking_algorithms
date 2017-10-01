object Factorial {

  def fact(x: Int): Int = {
    if (x == 1) 1
    else x * fact(x - 1)
  }

  // An implementation using tail-recursion to avoid stack-overflows
  def factTailRec(x: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, acc: Int): Int = {
      if (i == 1) acc
      else loop(i - 1, i * acc)
    }

    loop(x, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fact(5))
    println(factTailRec(5))
  }

}
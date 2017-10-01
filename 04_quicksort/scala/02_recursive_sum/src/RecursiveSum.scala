object RecursiveSum {

  def sum(arr: Seq[Int]): Int = arr match {
    case Seq() => 0
    case h :: t => h + sum(t)
  }

  def sumTailRec(arr: Seq[Int]): Int = {
    @annotation.tailrec
    def loop(remaining: Seq[Int], acc: Int): Int = remaining match {
      case Seq() => acc
      case h :: t => loop(t, h + acc)
    }

    loop(arr, 0)
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3, 4)))
    println(sumTailRec(List(1, 2, 3, 4)))
  }

}
object RecursiveCount {

  def count[A](list: Seq[A]): Int = list match {
    case Seq() => 0
    case h :: t => 1 + count(t)
  }

  def countTailRec[A](list: Seq[A]): Int = {
    @annotation.tailrec
    def loop(remaining: Seq[A], acc: Int): Int = remaining match {
      case Seq() => acc
      case h :: t => loop(t, 1 + acc)
    }

    loop(list, 0)
  }

  def main(args: Array[String]): Unit = {
    println(count(List(1, 2, 3, 4)))
    println(countTailRec(List(1, 2, 3, 4)))
  }
}
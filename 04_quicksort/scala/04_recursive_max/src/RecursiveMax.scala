object RecursiveMax {

  def max[A](list: Seq[A])(implicit ord: Ordering[A]): A = {
    list match {
      case Seq() => throw new IllegalArgumentException("Max of empty list")
      case h :: Seq() => h
      case h :: t => ord.max(h, max(t))
    }
  }

  def maxTailRec[A](list: Seq[A])(implicit ord: Ordering[A]): A = {
    @annotation.tailrec
    def loop(remaining: Seq[A], currMax: A): A = remaining match {
      case Seq() => currMax
      case h :: t => loop(t, ord.max(h, currMax))
    }

    if (list.isEmpty) throw new IllegalArgumentException("Max of empty list")
    loop(list.tail, list.head)

  }

  def main(args: Array[String]): Unit = {
    println(max(List(1, 9, 3, 4)))
    println(maxTailRec(List(1, 9, 2, 3, 4)))
  }

}
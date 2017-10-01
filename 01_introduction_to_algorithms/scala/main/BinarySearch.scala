object BinarySearch {

  // Binary search on indexed sequences, type parameter must have ordering trait so it works for general types
  // https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds
  def binarySearch[A](col: IndexedSeq[A], item: A)(implicit ord: Ordering[A]): Option[A] = {
    import ord.mkOrderingOps

    @annotation.tailrec
    def loop(low: Int, high: Int): Option[A] = {
      val mid = (low + high) / 2
      val guess = col(mid)
      if (guess == item) Some(guess)
      else if (low >= high) None
      else if (guess < item) loop(mid + 1, high)
      else loop(low, mid - 1)
    }

    loop(0, col.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val list = Array(1, 3, 5, 7, 9)
    println(binarySearch(list, 3))
    println(binarySearch(list, -1))
  }
}

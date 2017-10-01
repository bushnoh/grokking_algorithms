import scala.collection.LinearSeq

object SelectionSort {

  private def findLargest[A](col: LinearSeq[A])(implicit ord: Ordering[A]): (A, LinearSeq[A]) = {
    import ord.mkOrderingOps

    col.tail.foldLeft((col.head, LinearSeq.empty[A])) { (z, a) =>
      val curr = z._1
      if (a > curr) (a, curr +: z._2)
      else (curr, a +: z._2)
    }
  }

  // Do the selection sort in reverse, finding the highest each time and appending to the front as this is
  // cheap for LinearSeq
  def selectionSort[A](col: LinearSeq[A])(implicit ord: Ordering[A]): LinearSeq[A] = {

    @annotation.tailrec
    def loop(unsorted: LinearSeq[A], sorted: LinearSeq[A]): LinearSeq[A] = {
      if (unsorted.isEmpty) sorted
      else {
        val (high, remainder) = findLargest(unsorted)
        loop(remainder, high +: sorted)
      }
    }

    loop(col, LinearSeq.empty)
  }

  def main(args: Array[String]): Unit = {
    println(selectionSort(List(5, 3, 6, 2, 10)))
    println(selectionSort(List(5, 3, 6, 2, 10, 5, 3)))
    println(selectionSort(List()))
  }

}

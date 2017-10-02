object Quicksort {

  def splitByPivot[A](list: IndexedSeq[A])(implicit ord: Ordering[A]): (A, IndexedSeq[A], IndexedSeq[A]) = {
    val pivotIndex = scala.util.Random.nextInt(list.length)
    val pivot = list(pivotIndex)
    val (less, greater) = list.indices.filterNot(_ == pivotIndex).foldLeft((IndexedSeq.empty[A], IndexedSeq.empty[A])) { (z, i) =>
      val curr = list(i)
      if (ord.lt(curr, pivot)) (curr +: z._1, z._2)
      else (z._1, curr +: z._2)
    }
    (pivot, less, greater)
  }

  def quicksort[A](list: IndexedSeq[A])(implicit ord: Ordering[A]): IndexedSeq[A] = list match {
    case IndexedSeq() | IndexedSeq(_) => list
    case _ =>
      val (pivot, less, greater) = splitByPivot(list)
      quicksort(less :+ pivot) ++ quicksort(greater)
  }

  // Tail-recursive solution. Need to pass through work between calls so build a stack-like list containing work
  def quicksortTailRec[A](list: IndexedSeq[A])(implicit ord: Ordering[A]): IndexedSeq[A] = {
    @annotation.tailrec
    def loop(stack: List[IndexedSeq[A]], acc: IndexedSeq[A]): IndexedSeq[A] = stack match {
      case List() => acc
      case h :: tail => h match {
        case IndexedSeq() | IndexedSeq(_) => loop(tail, h ++ acc)
        case _ =>
          val (pivot, less, greater) = splitByPivot(h)
          loop(greater :: (less:+pivot) :: tail, acc)
      }
    }

    loop(List(list), IndexedSeq.empty[A])
  }

  def main(args: Array[String]): Unit = {
    println(quicksort(IndexedSeq(10, 5, 2, 3, 6, 2, 8, 8, 2)))
    println(quicksortTailRec(IndexedSeq(10, 5, 2, 3, 6, 2, 8, 8, 2)))
  }

}


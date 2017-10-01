object LoopSum {

  def sum(arr: Seq[Int]): Int = {
    var total = 0
    arr.foreach(total += _)
    total
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3, 4)))
  }

}
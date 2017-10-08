object PriceOfGroceries {

  val book = Map("apple" -> 0.67, "milk" -> 1.49, "avocado" -> 1.49)

  def main(args: Array[String]): Unit = {
    println(book)
    println(book("avocado"))
  }
}
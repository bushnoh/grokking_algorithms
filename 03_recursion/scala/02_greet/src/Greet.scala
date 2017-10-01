object Greet {

  def greet2(name: String): Unit = println(s"how are you, $name?")

  def bye(): Unit = println("ok bye!")

  def greet(name: String): Unit = {
    println(s"hello, $name!")
    greet2(name)
    println("getting ready to say bye...")
    bye()
  }

  def main(args: Array[String]): Unit = {
    greet("adit")
  }

}
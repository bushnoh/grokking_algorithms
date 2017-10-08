import scala.collection.immutable.Queue

object BreadthFirstSearch {

  def isSeller(name: String): Boolean = name.endsWith("m")

  def search(graph: Map[String, List[String]], name: String): Boolean = {

    @annotation.tailrec
    def loop(queue: Queue[String], searched: List[String]): Boolean = queue.dequeueOption match {
      case None => false
      case Some((person, newQueue)) =>
        if (searched.contains(person)) loop(newQueue, searched)
        else if (isSeller(person)) true
        else
          loop(newQueue.enqueue(graph(person)), person :: searched)
    }

    loop(Queue().enqueue(graph(name)), List())
  }

  def main(args: Array[String]): Unit = {

    val graph = Map(
      "you" -> List("alice", "bob", "claire"),
      "bob" -> List("anuj", "peggy"),
      "alice" -> List("peggy"),
      "claire" -> List("thom", "jonny"),
      "anuj" -> List(),
      "peggy" -> List(),
      "thom" -> List(),
      "jonny" -> List()
    )

    println(search(graph, "you"))

  }

}
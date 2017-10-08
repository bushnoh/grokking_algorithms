object DijkstrasAlgorithm {

  type Graph = Map[String, Map[String, Int]]
  type Results = Map[String, (Int, Option[String])]

  def initialiseResults(graph: Graph, start: String): Results = {
    graph.foldLeft(Map(): Results)((z, e) =>
      if (e._1 == start) z
      else
        graph(start).get(e._1) match {
          case Some(i) => z + (e._1 -> (i, Some(start)))
          case None => z + (e._1 -> (Integer.MAX_VALUE, None))
        })
  }

  def findLowestCost(costs: Results, processed: List[String]): Option[String] = {
    costs.foldLeft((Integer.MAX_VALUE, None: Option[String]))((z, e) =>
      if (e._2._1 < z._1 && !processed.contains(e._1)) (e._2._1, Some(e._1))
      else z
    )._2
  }

  def dijkstrasAlgorithm(graph: Graph, start: String): Results = {

    def loop(table: Results, processed: List[String]): Results = findLowestCost(table, processed) match {
      case None => table
      case Some(nodeName) =>
        val cost = table(nodeName)._1
        val neigbours = graph(nodeName)
        val newTable = neigbours.foldLeft(table) { (z, e) =>
          val newCost = cost + neigbours(e._1)
          if (newCost < table(e._1)._1) z + (e._1 -> (newCost, Some(nodeName)))
          else z
        }
        loop(newTable, nodeName :: processed)
    }

    val table = initialiseResults(graph, start)
    loop(table, List())
  }

  def main(args: Array[String]): Unit = {

    val graph: Graph = Map(
      "start" -> Map(
        "a" -> 6,
        "b" -> 2
      ),
      "a" -> Map(
        "fin" -> 1
      ),
      "b" -> Map(
        "a" -> 3,
        "fin" -> 5
      ),
      "fin" -> Map()
    )

    println(dijkstrasAlgorithm(graph, "start"))

  }

}
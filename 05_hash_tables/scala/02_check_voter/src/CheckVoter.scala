object CheckVoter {

  case class VoteRegister(voted: Map[String, Boolean] = Map()) {

    def checkVoter(name: String): VoteRegister = voted.get(name) match {
      case Some(voted) => println("kick them out!"); this
      case None => println("let them vote!"); VoteRegister(voted + (name -> true))
    }
  }

  def main(args: Array[String]): Unit = {

    val register = List("tom", "mike", "mike").foldLeft(VoteRegister())((z, a) => z.checkVoter(a))

  }

}
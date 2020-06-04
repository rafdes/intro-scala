package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || c==r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    /** V2 - funzionante, uso la lista direttamente */
    def balanceEsegui(appChars:List[Char],numPar:Int=0):Boolean = {
      if (numPar<0) false
      else
        appChars match {
          case Nil     => numPar==0
          case '('::xs => balanceEsegui(appChars.tail, numPar+1)
          case ')'::xs => balanceEsegui(appChars.tail, numPar-1)
          case _::xs   => balanceEsegui(appChars.tail, numPar)
        }
    }

    balanceEsegui(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /** No gettoni/no schei */
    if (coins.isEmpty || money<0) 0
    else
      /** Taglio di soldi perfetto */
      if (money==0) 1
      else
        /** cambio con il primo taglio e sottraggo il taglio al numero,
         *  richiamo la funzione sui tagli successivi  con il numero per intero */
        countChange(money-coins(0), coins) + countChange(money, coins.tail)
  }
}

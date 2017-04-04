package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
  if (c == 0 || c == r)
    1
  else
    pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balance(chars: List[Char], counter: Int): Boolean = {
      if (chars.isEmpty)
        counter == 0
      else if (counter < 0)
        false
      else if (chars.head == '(')
        balance(chars.tail, counter + 1)
      else if (chars.head == ')')
        balance(chars.tail, counter - 1)
      else
        balance(chars.tail, counter)
    }
    balance(chars, 0)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0)
      0
    else if (money == 0)
      1
    else
      coins match {
        case Nil => 0
        case c => countChange(money - c.head, c) + countChange(money, c.tail)
      }
  }

  //    if (money > 0)
  //      if (coins.isEmpty)
  //        0
  //      else {
  //        countChange(money - coins.head, coins) +
  //        countChange(money, coins.tail)
  //      }
  //    else if (money < 0)
  //      0
  //    else
  //      1
  //  }
}

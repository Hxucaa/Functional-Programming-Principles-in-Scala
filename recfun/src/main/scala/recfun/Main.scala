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
    def pascal(c: Int, r: Int): Int = {
      (c, r) match {
        case (0, _) => 1
        case (_, 0) => 1
        case (c, r) if c == r => 1
        case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = balance(chars, 0)

  private def balance(chars: List[Char], count: Int): Boolean = {
    if (chars.isEmpty) return count == 0

    val tail = chars.tail
    chars.head match {
      case '(' => balance(tail, count + 1)
      case ')' if count > 0 => balance(tail, count - 1)
      case ')' => false
      case _ => balance(tail, count)
    }
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else if (money <= 0 && coins.nonEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

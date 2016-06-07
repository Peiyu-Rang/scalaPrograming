package recfun

import scala.annotation._
object Main {
  def main(args: Array[String]): Unit = {
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
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def r(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty || openCount < 0) {
        openCount == 0
      } else {

        val head = chars.head
        val a = head match {
          case '(' => openCount + 1
          case ')' => openCount + 1
          case _   => openCount
        }
        r(chars.tail, a)
      }
    }
    r(chars, 0)

    /****
    def r(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty || openCount < 0) {
        openCount == 0
      } else {
        val a =
          if (chars.head == '(') {
            openCount + 1
          } else if (chars.head == ')') {
            openCount - 1
          } else {
            openCount
          }
        r(chars.tail, a)
      }
    }

    r(chars, 0)
    */
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def r(money: Int, num_coins: Int, coins: List[Int]): Int = {
      if(money == 0) 1 else{
        if(money < 0 || num_coins <=0) 0 else {
          r(money, (coins.length -1 ), coins) + r((money - coins(coins.length -1)),  coins.length, coins)
        }
      }
    }
    
    r(money, coins.length, coins)
  }
}

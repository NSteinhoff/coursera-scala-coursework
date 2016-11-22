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
      if(c == 0 || r == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inner(acc: Int, cs: List[Char]): Boolean = cs match {
        case Nil => acc == 0
        case x::xs => {
          if (acc < 0) false
          else x match {
            case '(' => inner(acc+1, xs)
            case ')' => inner(acc-1, xs)
            case _ => inner(acc, xs)
          }
        }
      }
      inner(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (0, _) => 1
      case (_, Nil) => 0
      case (m, _) if m < 0 => 0
      case (m, c::cs) => countChange(m-c, coins) + countChange(m, cs)
    }
  }

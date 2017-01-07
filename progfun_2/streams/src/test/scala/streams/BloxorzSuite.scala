package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block = {
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("level 1, startBlock neighbors") {
    new Level1 {
      assert(
        startBlock.legalNeighbors ===
          List((startBlock.right, Right),
               (startBlock.down, Down))
      )
    }
  }

  test("level 1, block legality") {
    new Level1 {
      assert(Block(Pos(2, 4), Pos(3, 4)).isLegal, "(2,4), (3,4)")
      assert(!Block(Pos(4, 4), Pos(4, 4)).isLegal, "(4,4), (4,4)")
    }
  }


  test("level 1, block neighbors") {
    new Level1 {
      val b = Block(Pos(2, 4), Pos(3, 4))
      assert(
        b.legalNeighbors ===
          List((b.up, Up),
               (b.right, Right),
               (b.left, Left))
      )
    }
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(4,4)), "4,4")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

  test("findChar level 1, start") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


  test("findChar level 1, goal") {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}

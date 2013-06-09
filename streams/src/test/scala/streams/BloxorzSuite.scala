package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level0 extends SolutionChecker {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
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
  
  trait Level6 extends SolutionChecker {

    val level = 
      """-----oooooo
      |-----o--ooo
      |-----o--ooooo
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo
      |------ooooo
      |------ooooo
      |-------ooo""".stripMargin
  }

  test("findChar level 0") {
    new Level0 {
      assert(!terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(0,1)), "0,1")
      assert(startPos == Pos(1,2))
      assert(goal == Pos(1,3))
    }
  }
  
  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }
  
  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4, 7))
    }
  }
  
  test("findChar level 6") {
    new Level6 {
      assert(!terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(0,1)), "0,1")
      assert(startPos == Pos(3,0))
      assert(goal == Pos(4, 13))
    }
  }

  test("neighborsWithHistory level 0") {
    new Level0 {
      val neighborOfS = neighborsWithHistory(Block(Pos(1,2),Pos(1,2)), List())
      assert(neighborOfS.toSet == Set((Block(Pos(2,2),Pos(3,2)), List(Down))))
      val neighborOfT = neighborsWithHistory(Block(Pos(1,3),Pos(1,3)), List())
      assert(neighborOfT.toSet == Set((Block(Pos(2,3),Pos(3,3)), List(Down))))
      val neighbor1 = neighborsWithHistory(Block(Pos(2,2),Pos(3,2)), List())
      assert(neighbor1.toSet == Set((Block(startPos, startPos), List(Up)),
          (Block(Pos(2,3), Pos(3,3)), List(Right))))
    }
  }
  
  test("neighborsWithHistory level 1") {
    new Level1 {
      val neighbors = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      assert(neighbors.toSet == Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
    }
  }
  
  test("neighborsWithHistory level 6") {
    new Level6 {
      val neighbors = neighborsWithHistory(Block(Pos(4,5), Pos(5,5)), List())
      assert(neighbors.toSet == Set((Block(Pos(4,4), Pos(5,4)), List(Left)),
          (Block(Pos(4,6), Pos(5,6)), List(Right)), (Block(Pos(3,5), Pos(3,5)), List(Up))))
    }
  }

  test("newNeighborsOnly level 1") {
    new Level1 {
      val newNeighbors = newNeighborsOnly(
          Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
              (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream,
          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1))))
      assert(newNeighbors == Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)
      val newNeighbors2 = newNeighborsOnly(
          Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
              (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream, Set())
      assert(newNeighbors2 == Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
              (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)
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

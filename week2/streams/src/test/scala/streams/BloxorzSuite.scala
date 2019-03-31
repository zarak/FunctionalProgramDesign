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


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding false when block is flat") {
    new Level1 {
      val p1 = Pos(1, 0)
      val p2 = Pos(2, 0)
      assert(Block(p1, p2).isStanding === false)
    }
  }

  test("isStanding true when block is upright") {
    new Level1 {
      val p1 = Pos(1, 0)
      val p2 = Pos(1, 0)
      assert(Block(p1, p2).isStanding === true)
    }
  }

  test("isLegal") {
    new Level1 {
      val p1 = Pos(1, 0)
      val p2 = Pos(1, 0)
      val p3 = Pos(0, 4)
      val p4 = Pos(0, 5)
      val p5 = Pos(0, 2)
      val p6 = Pos(0, 3)
      val b1 = Block(p1, p2)
      val b2 = Block(p3, p4)
      val b3 = Block(p5, p6)
      assert(b1.isLegal)
      assert(!b2.isLegal)
      assert(!b3.isLegal, "half of block is on terrain")
    }
  }

  test("starting block") {
    new Level1 {
      val b1 = Block(startPos, startPos)
      val b2 = Block(goal, goal)
      assert(b1 == Block(Pos(1, 1), Pos(1, 1)))
      assert(b2 != Block(Pos(1, 1), Pos(1, 1)))
    }
  }

  test("neighbors") {
    new Level1 {
      val b1 = Block(startPos, startPos)
      assert(b1.neighbors ===
        List(
          (Block(Pos(1,-1),Pos(1,0)),Left), (Block(Pos(1,2),Pos(1,3)),Right),
          (Block(Pos(-1,1),Pos(0,1)),Up), (Block(Pos(2,1),Pos(3,1)),Down))
        )
    }
  }

  test("legal neighbors") {
    new Level1 {
      val b1 = Block(startPos, startPos)
      assert(b1.legalNeighbors ===
        List(
          (Block(Pos(1,2),Pos(1,3)),Right),
          (Block(Pos(2,1),Pos(3,1)),Down))
      )
    }
  }

  test("block at goal is done") {
    new Level1 {
      val b = Block(goal, goal)
      assert(done(b))
    }
  }

  test("neighbors with history") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet == Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }
  }

  test("new neighbors only") {
    new Level1 {
      assert(
        newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1))) ) ==
          Set(
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).toStream
      )
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

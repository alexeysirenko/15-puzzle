import Board.BoardMatrix
import org.scalatest.{FunSuite, Matchers}

trait BoardHelpers { self: FunSuite with Matchers =>

  test("Self-test test matrix creation") {
    createTestBoardMatrix(1, 0, 0) shouldBe Vector(
      Vector(Board.EMPTY_BLOCK)
    )
    createTestBoardMatrix(2, 1, 1) shouldBe Vector(
      Vector(1, 2),
      Vector(3, Board.EMPTY_BLOCK)
    )
    createTestBoardMatrix(3, 2, 2) shouldBe Vector(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, Board.EMPTY_BLOCK)
    )
  }

  def createTestBoardMatrix(size: Int, emptyX: Int, emptyY: Int): BoardMatrix  = {
    require(emptyX < size && emptyY < size && emptyX >=0 && emptyY >= 0)
    val dices = (1 until size * size)
      .foldLeft(List.empty[Int])((acc, value) => value :: acc)
      .reverse
    val (first, second) = dices.splitAt(emptyX + emptyY * size)
    (first ::: (Board.EMPTY_BLOCK :: second)).toVector.sliding(size, size).toVector
  }

}

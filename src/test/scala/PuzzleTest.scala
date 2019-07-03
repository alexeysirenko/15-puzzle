import org.scalatest.{FunSuite, Matchers}

class PuzzleTest extends FunSuite with Matchers with BoardHelpers {

  test("Update board correctly") {
    val board = Board(createTestBoardMatrix(size = 2, emptyX = 0, emptyY = 0))

    Puzzle.update(Puzzle.moveRight)(board).matrix shouldBe Vector(Vector(1, Board.EMPTY_BLOCK), Vector(2, 3))
    Puzzle.update(Puzzle.moveDown)(board).matrix shouldBe Vector(Vector(2, 1), Vector(Board.EMPTY_BLOCK, 3))
    Puzzle.update(Puzzle.moveLeft)(board).matrix shouldBe board.matrix
    Puzzle.update(Puzzle.moveUp)(board).matrix shouldBe board.matrix
  }

}

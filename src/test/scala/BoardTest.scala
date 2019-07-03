import Board.BoardMatrix
import org.scalatest.{AppendedClues, FunSuite, Matchers, OptionValues}

class BoardTest extends FunSuite with Matchers with OptionValues with AppendedClues with BoardHelpers {

  test("Not allow creation of empty board") {
    an [Exception] should be thrownBy Board(Vector.empty)
    an [Exception] should be thrownBy Board(Vector(Vector.empty))
  }

  test("Not allow creation of a board without empty dice block") {
    val matrix = Vector(
      Vector(1, 2),
      Vector(3, 4)
    )

    an [Exception] should be thrownBy Board(matrix)
  }

  test("Not allow creation of asymmetrical board") {
    val matrix = Vector(
      Vector(1, Board.EMPTY_DICE),
      Vector(3),
      Vector(4, 5, 6)
    )
    an [Exception] should be thrownBy Board(matrix)
  }

  test("Find empty dice") {
    val matrix1 = createTestBoardMatrix(size = 2, emptyX = 1, emptyY = 1)
    val matrix2 = createTestBoardMatrix(size = 3, emptyX = 2, emptyY = 0)

    Board(matrix1).emptyDiceXY shouldBe (1, 1)
    Board(matrix2).emptyDiceXY shouldBe (2, 0)
  }

  test("Move empty dice down") {
    val initialBoard = Board(createTestBoardMatrix(size = 3, emptyX = 1, emptyY = 0))

    val movedOnceBoard = initialBoard.moveEmptyDice(Puzzle.moveDown).value
    val movedTwiceBoard = movedOnceBoard.moveEmptyDice(Puzzle.moveDown).value

    movedOnceBoard.matrix shouldBe Vector(Vector(1, 4, 2), Vector(3, Board.EMPTY_DICE, 5), Vector(6, 7, 8))
    movedTwiceBoard.matrix shouldBe Vector(Vector(1, 4, 2), Vector(3, 7, 5), Vector(6, Board.EMPTY_DICE, 8))
  }

  test("Move empty dice up") {
    val initialBoard = Board(createTestBoardMatrix(size = 3, emptyX = 2, emptyY = 2))

    val movedOnceBoard = initialBoard.moveEmptyDice(Puzzle.moveUp).value
    val movedTwiceBoard = movedOnceBoard.moveEmptyDice(Puzzle.moveUp).value

    movedOnceBoard.matrix shouldBe Vector(Vector(1, 2, 3), Vector(4, 5, Board.EMPTY_DICE), Vector(7, 8, 6))
    movedTwiceBoard.matrix shouldBe Vector(Vector(1, 2, Board.EMPTY_DICE), Vector(4, 5, 3), Vector(7, 8, 6))
  }

  test("Move empty dice right") {
    val initialBoard = Board(createTestBoardMatrix(size = 3, emptyX = 0, emptyY = 1))

    val movedOnceBoard = initialBoard.moveEmptyDice(Puzzle.moveRight).value
    val movedTwiceBoard = movedOnceBoard.moveEmptyDice(Puzzle.moveRight).value

    movedOnceBoard.matrix shouldBe createTestBoardMatrix(size = 3, emptyX = 1, emptyY = 1)
    movedTwiceBoard.matrix shouldBe createTestBoardMatrix(size = 3, emptyX = 2, emptyY = 1)
  }

  test("Move empty dice left") {
    val initialBoard = Board(createTestBoardMatrix(size = 3, emptyX = 2, emptyY = 1))

    val movedOnceBoard = initialBoard.moveEmptyDice(Puzzle.moveLeft).value
    val movedTwiceBoard = movedOnceBoard.moveEmptyDice(Puzzle.moveLeft).value

    movedOnceBoard.matrix shouldBe createTestBoardMatrix(size = 3, emptyX = 1, emptyY = 1)
    movedTwiceBoard.matrix shouldBe createTestBoardMatrix(size = 3, emptyX = 0, emptyY = 1)
  }

  test("Handle moving of an empty dice out of bounds") {
    val initialBoard = Board(createTestBoardMatrix(size = 1, emptyX = 0, emptyY = 0))

    initialBoard.moveEmptyDice(Puzzle.moveLeft) shouldBe None
    initialBoard.moveEmptyDice(Puzzle.moveRight) shouldBe None
    initialBoard.moveEmptyDice(Puzzle.moveUp) shouldBe None
    initialBoard.moveEmptyDice(Puzzle.moveDown) shouldBe None
  }

  test("Restore initial state after moving around the board") {
    val initialBoard = Board(createTestBoardMatrix(size = 2, emptyX = 0, emptyY = 0))
    val movesForward = List(Puzzle.moveRight, Puzzle.moveDown, Puzzle.moveLeft, Puzzle.moveUp)
    val movesBack = List(Puzzle.moveLeft, Puzzle.moveUp,Puzzle.moveRight, Puzzle.moveDown).reverse

    (movesForward ++ movesBack).foldLeft(initialBoard) { (acc, move) =>
      acc.moveEmptyDice(move).value
    } shouldBe initialBoard
  }

  test("Handle completed puzzle") {
    val initialBoard = Board(createTestBoardMatrix(size = 2, emptyX = 0, emptyY = 1))
    initialBoard.isValid shouldBe false
    val movedBoardOnce = initialBoard.moveEmptyDice(Puzzle.moveRight).value
    movedBoardOnce.isValid shouldBe true
    val movedBoardTwice = initialBoard.moveEmptyDice(Puzzle.moveUp).value
    movedBoardTwice.isValid shouldBe false
  }

  test("Create randomized board correctly") {
    val boardSize = 4
    val seeds = List(0L, 42L, 1L, 999L, 81038104L)
    val randomizedBoards = seeds.map(seed => Board.createBoard(randomSeed = seed, boardSize))

    randomizedBoards.foreach { board =>
      board.matrix.size shouldBe boardSize
      board.matrix.foreach(row => row.size shouldBe boardSize)

      val allDices = board.matrix.reduce(_ ++ _)
      allDices.max shouldBe boardSize * boardSize -1

      val uniqueDices = allDices.toSet
      uniqueDices.size shouldBe boardSize * boardSize

      allDices.toList.sliding(2).exists { pair =>
        pair match {
          case prev :: next :: Nil => prev > next
          case _ => false
        }
      } shouldBe true withClue "Randomized dices should be rearranged"
    }
  }

  test("Show printable board representation correctly") {
    val obtained = Board(createTestBoardMatrix(2, 1, 1)).show
    val expected =
      s"|1|2|\n" +
      s"|3| |\n"

    obtained shouldBe expected
  }

}

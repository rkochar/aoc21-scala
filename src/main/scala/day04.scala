import scala.io.Source

/**
 * Solve bingo.
 */
class day04 {

    type Board = List[List[Int]]

    type MutateBoard = Array[Array[Int]]

    val input: List[String] = Source.fromFile("src/main/resources/day04.txt").mkString.split("\n").toList

    val strikes: List[Int] = input.head.split(",").map(_.toInt).toList

    val boards: List[Board] = input.drop(2).filterNot(x => x.equals("")).map(x => x.split(",")(0).replace(" ", ",").split(",")).map(x => x.filterNot(_.isBlank).map(_.toInt).toList).sliding(5, 5).map(x => x: Board).toList

    var boardsMutate: Array[(MutateBoard, Int)] = input.drop(2).filterNot(x => x.equals("")).map(x => x.split(",")(0).replace(" ", ",").split(",")).toArray.map(x => x.filterNot(_.isBlank).map(_.toInt).toArray).sliding(5, 5).map(x => x: MutateBoard).toArray.zipWithIndex

    def part1(i: Int): Int = {
        if (i == strikes.length) -1
        else if (checkBoards(boards, i) == -1) part1(i + 1)
        else -1
    }

    def checkBoards(boards: List[Board], i: Int): Int = boards match {
        case Nil => -1
        case head :: tail => {
            val currentStrikes = strikes.take(i)
            val rowValue = checkRows(head, 0, currentStrikes)
            val columnValue = checkRows(head.transpose, 0, currentStrikes)

            if (rowValue != -1) rowValue
            else if (columnValue != -1) columnValue
            else checkBoards(tail, i)
        }
    }

    def findScore(board: Board, strikes: List[Int], lastNumber: Int): Int = (board.flatten.sum - board.flatten.intersect(strikes).sum) * lastNumber

    def checkRows(board: Board, index: Int, strikes: List[Int]): Int = {
        if (index >= 5)                                    -1
        else if (board(index).forall(strikes.contains(_))) {
            val v = findScore(board, strikes, strikes.takeRight(1).head)
            println(v)
            v
        }
        else checkRows(board, index + 1, strikes)
    }

    def checkBoardsPart2(boards: List[Board], i: Int): List[Board] = boards match {
        case Nil          => Nil
        case head :: tail => {
            val currentStrikes = strikes.take(i)
            val rowValue = checkRows(head, 0, currentStrikes)
            val columnValue = checkRows(head.transpose, 0, currentStrikes)

            if (rowValue != -1 || columnValue != -1) {
                tail
            }
            else head :: checkBoardsPart2(tail, i)
        }
    }

    def part2(i: Int, boardsPart2: List[Board]): Int = {
        if (i == strikes.length) -1

        boardsPart2 match {
            case Nil         => -1
            case head :: Nil => findScore(head, strikes.take(i), strikes.take(i).takeRight(1).head)
            case _           => part2(i + 1, checkBoardsPart2(boardsPart2, i))
        }
    }

    def strikeOut(board: MutateBoard, inp: Int): Unit = {
        for (b <- board)
            for (j <- Range(0, b.length, 1))
                if (b(j) == inp)
                    b(j) = -1
    }

    def checkWinningBoard(board: MutateBoard): Boolean = {
        for (b <- board) if (b.count(x => x == -1) == board.length) return true
        false
    }

    def delete[A](nth: Int): Unit = {
        this.boardsMutate = this.boardsMutate.filter(_._2 != nth)
    }

    def checkMutateBoard(board: MutateBoard, index: Int): Boolean = {
        if (checkWinningBoard(board)) {
            delete(index)
            return true
        }
        false
    }

    def findScore(board: MutateBoard, lastNumber: Int): Int = board.flatten.filter(x => x != -1).sum * lastNumber

    def part2iterative(input: List[Int]): Int = {
        for (inp <- input) {
            for (board <- this.boardsMutate) {
                if (this.boardsMutate.length > 1) {
                    strikeOut(board._1, inp)
                    if (!checkMutateBoard(board._1, board._2)) checkMutateBoard(board._1.transpose, board._2)
                }
            }
            if (this.boardsMutate.length == 1) {
                strikeOut(this.boardsMutate.head._1, inp)
                if (checkWinningBoard(this.boardsMutate.head._1) || checkWinningBoard(this.boardsMutate.head._1.transpose))
                    return findScore(this.boardsMutate.head._1, inp)
            }
        }
        0
    }

    def run(): Unit = {
        println("Part 1: " + part1(1)) // Prints correct answer but does not it. Very strange!!!
        println("Part 2: " + part2iterative(strikes))
    }

}

import scala.io.Source;

class day2 {

    def part1helper(t: (String, Int)): (Int, Int) = t match {
        case ("forward", n) => (n, 0)
        case ("up", n)      => (0, -n)
        case ("down", n)    => (0, n)
        case _              => (0, 0)
    }

    def part2helper(t: (String, Int), aim: Int): (Int, Int, Int) = t match {
        case ("forward", n) => (n, aim * n, aim)
        case ("up", n)      => (0, 0, aim - n)
        case ("down", n)    => (0, 0, aim + n)
        case _              => (0, 0, 0)
    }

    def part1(xs: List[(String, Int)]): Int = {
        val coordinates = xs.map(part1helper).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
        coordinates._1 * coordinates._2
    }

    def part2(xs: List[(String, Int)]): Int = {
        def inner(xs: List[(String, Int)], aim: Int): List[(Int, Int)] = xs match {
            case head :: tail => {
                val thisValue = part2helper(head, aim)
                (thisValue._1, thisValue._2) :: inner(tail, thisValue._3)
            }
            case _            => List.empty
        }

        val coordinates = inner(xs, 0).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
        coordinates._1 * coordinates._2
    }

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day2.txt").mkString.linesIterator.map(_.split(" ")).map(x => (x(0), x(1).toInt)).toList
        println("Part 1 is: " + part1(input))
        println("Part 2 is: " + part2(input))
    }
}

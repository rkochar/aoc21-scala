import scala.io.Source

/**
 * Find something similar to maxima and minima.
 */
class day01 {
    def print(): String = "string"

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day01.txt").mkString.linesIterator.map(_.toInt).toList
        println("Part 1 is: " + part1(input))
        println("Part 2 is: " + part2(input))
    }

    def part1(xs: List[Int]): Int = xs match {
        case Nil | _ :: Nil => 0
        case first :: second :: tail => if (second > first) {
            part1(second :: tail) + 1
        } else {
            part1(second :: tail)
        }
        case _ => 0
    }

    def part2(xs: List[Int]): Int = xs match {
        case first :: second :: third :: fourth :: tail => if (first < fourth) {
            1 + part2(second :: third :: fourth :: tail)
        } else {
            part2(second :: third :: fourth :: tail)
        }
        case _ => 0
    }
}

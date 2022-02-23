import scala.io.Source

/**
 * Crab engineering - find best alignment spot for crabs (elements of array).
 */
class day07 {

    val input: List[Int] = Source.fromFile("src/main/resources/day07.txt").mkString.split(",").map(_.toInt).toList

    def part1(xs: List[Int]): Int = {
        val median: Int = findMedian(xs)
        Integer.min(distance(xs, median), distance(xs, median + 1))
    }

    def distance(xs: List[Int], point: Int): Int = xs.map(x => Math.abs(x - point)).sum

    def findMedian(s: List[Int]): Int  = {
        val (lower, upper) = s.sortWith(_ < _).splitAt(s.size / 2)
        if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
    }

    def part2(xs: List[Int]): Int = {
        val mean: Int = (xs.sum / xs.length)
        Integer.min(cumulativeDistance(xs, mean), cumulativeDistance(xs, mean + 1))
    }

    def cumulativeDistance(xs: List[Int], point: Int): Int = xs.map(x => {
        val distance = Math.abs(x - point)
        distance * (distance + 1) / 2
    }).sum

    def run(): Unit = {
        println("Part 1: " + part1(input))
        println("Part 2: " + part2(input))
    }
}

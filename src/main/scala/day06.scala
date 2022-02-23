import scala.+:
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Calculate population of lanternfish at d days.
 */
class day06 {

    // back to fp :)
    def part1(xs: List[Int], i: Int, days: Int): Int = {
        def inner(xs: List[Int], i: Int, children: List[Int]): Int = xs match {
            case Nil          => part1(children, i + 1, days)
            case head :: tail => if (head == 0) inner(tail, i, 6 :: 8 :: children)
                                 else inner(tail, i, head - 1 :: children)
        }

        if (i == days) xs.length
        else inner(xs, i, List.empty)
    }

    def update_day(xs: Array[Long]): Array[Long] = {
        val born = xs(0)
        Range(0, 8).foreach(i => xs(i) = xs(i + 1))
        xs(8) = born
        xs(6) += born
        xs
    }

    def part2(input: List[Int]): Long = {
        val xs: Array[Long] = Array.ofDim[Long](9)
        val inputMap = input.groupBy(x => x).map(x => (x._1, x._2.length)).foreach(x => xs(x._1) = x._2)

        @tailrec
        def inner(xs: Array[Long], day: Int): Array[Long] = {
            if (day < 256) inner(update_day(xs), day + 1)
            else xs
        }

        inner(xs, 0).sum
    }

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day06.txt").mkString.replace("\"", "").split(",").map(_.toInt).toList

        println("Part 1: " + part1(input, 0, 80))
        println("Part 2: " + part2(input))
    }
}

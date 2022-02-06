import scala.io.Source

/**
 * Find intersection of lines. Gave up on fp here. Sometimes having a mutable data structure can prevent me from pulling hear out of my head.
 */
class day5 {

    type Point = (Int, Int)
    type Line = (Point, Point)

    var grid1: Array[Array[Int]] = Array.ofDim[Int](1000, 1000)
    var grid2: Array[Array[Int]] = Array.ofDim[Int](1000, 1000)

    def toPair[A](xs: Seq[A]): Seq[(A, A)] = xs.zip(xs.tail)

    def stringToLine(string: String): Line = {
        val t = string.split("->").map(_.split(",").map(_.toInt)).map(toPair(_))
        ((t(0).head._1, t(0).head._2), (t(1).head._1, t(1).head._2))
    }

    val input: List[Line] = Source.fromFile("src/main/resources/day5.txt").mkString.replace(" ", "").split("\n").map(stringToLine).toList

    val inputPart1: List[Line] = input.filter((x: Line) => (x._1._1 == x._2._1 || x._1._2 == x._2._2))

    def horizontalLine(from: Point, to: Point, grid: Array[Array[Int]]): Unit = if (from._2 < to._2) Range(from._2, to._2 + 1, 1).foreach(grid(from._1)(_) += 1)
                                                                                else                 Range(to._2, from._2 + 1, 1).foreach(grid(from._1)(_) += 1)

    def verticalLine(from: Point, to: Point, grid: Array[Array[Int]]): Unit = if (from._1 < to._1) Range(from._1, to._1 + 1, 1).foreach(grid(_)(from._2) += 1)
                                                                              else                 Range(to._1, from._1 + 1, 1).foreach(grid(_)(from._2) += 1)

    def part1(input: List[Line]): Int = input match {
        case head :: tail => {
            val from = head._1
            val to   = head._2
            if (from._1 == to._1) horizontalLine(from, to, grid1)
            else verticalLine(from, to, grid1)
            part1(tail)
        }
        case Nil          => grid1.map(x => x.map(y => if (y > 1) 1 else 0).sum).sum
    }

    def part2(input: List[Line]): Int = input match {
        case head :: tail => {
            val from = head._1
            val to   = head._2
            if (from._1 == to._1)      horizontalLine(from, to, grid2)
            else if (from._2 == to._2) verticalLine(from, to, grid2)
            else {
                if (from._1 < to._1 && from._2 < to._2)        {
                    val t = Range(from._1, to._1 + 1, 1).zip(Range(from._2, to._2 + 1, 1))
                        t.map(x => grid2(x._1)(x._2) += 1)
                }
                else if (from._1 >= to._1 && from._2 >= to._2) {
                    val t = Range(to._1, from._1 + 1, 1).zip(Range(to._2, from._2 + 1, 1))
                        t.map(x => grid2(x._1)(x._2) += 1)
                }
                else if (from._1 < to._1 && from._2 >= to._2) {
                    val t = Range(from._1, to._1 + 1, 1).zip(Range(from._2, to._2 - 1, -1))
                    t.map(x => grid2(x._1)(x._2) += 1)
                }
                else                                           {
                    val t = Range(from._1, to._1 - 1, -1).zip(Range(from._2, to._2 + 1, 1))
                        t.map(x => grid2(x._1)(x._2) += 1)
                }
            }
            part2(tail)
        }
        case Nil          => grid2.map(x => x.map(y => if (y > 1) 1 else 0).sum).sum
    }

    def run(): Unit = {
        println("Part 1: " + part1(inputPart1))
        println("Part 2: " + part2(input))
    }

}

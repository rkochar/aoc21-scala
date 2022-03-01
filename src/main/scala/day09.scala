import scala.io.Source

class day09 {

    def isCorner(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Boolean = (i == 0 && j == 0) || (i == 0 && j == xss.head._1.length - 1) || (i == xss.length - 1 && j == 0) || (i == xss.length - 1 && j == xss.head._1.length - 1)

    def isEdge(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Boolean = i == 0 || j == 0 || i == xss.length - 1 || j == xss.head._1.length - 1

    def checkCorner(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Int = {
        val check = xss(i)._1(j)._1
        // TODO: cleanup
        if (i == 0 && j == 0) {
            if (check < xss(i)._1(j + 1)._1 && check < xss(i + 1)._1(j)._1) check + 1
            else 0
        } else if (i == 0 && j == xss.head._1.length - 1) {
            if (check < xss(i)._1(j - 1)._1 && check < xss(i + 1)._1(j)._1) check + 1
            else 0
        } else if (i == xss.length - 1 && j == 0) {
            if (check < xss(i)._1(j + 1)._1 && check < xss(i - 1)._1(j)._1) check + 1
            else 0
        }
        else {
            if (check < xss(i)._1(j - 1)._1 && check < xss(i - 1)._1(j)._1) check + 1
            else 0
        }
    }

    def checkEdge(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Int = {
        val check = xss(i)._1(j)._1
        if (i == 0) {
            if (check < xss(i)._1(j - 1)._1 && check < xss(i)._1(j + 1)._1 && check < xss(i + 1)._1(j)._1) check + 1
            else 0
        } else if (j == 0) {
            if (check < xss(i - 1)._1(j)._1 && check < xss(i + 1)._1(j)._1 && check < xss(i)._1(j + 1)._1) check + 1
            else 0
        } else if (i == xss.length - 1) {
            if (check < xss(i)._1(j - 1)._1 && check < xss(i)._1(j + 1)._1 && check < xss(i - 1)._1(j)._1) check + 1
            else 0
        } else {
            if (check < xss(i - 1)._1(j)._1 && check < xss(i + 1)._1(j)._1 && check < xss(i)._1(j - 1)._1) check + 1
            else 0
        }
    }

    def checkSquare(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Int = {
        val check = xss(i)._1(j)._1
        if (check < xss(i)._1(j - 1)._1 && check < xss(i)._1(j + 1)._1 && check < xss(i - 1)._1(j)._1 && check < xss(i + 1)._1(j)._1) check + 1
        else 0
    }

    def part1check(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Int = {
        if (isCorner(xss, i, j))    checkCorner(xss, i, j)
        else if (isEdge(xss, i, j)) checkEdge(xss, i, j)
        else                        checkSquare(xss, i, j)
    }

    def part1(xss: List[List[Int]]): Int = {
        val zipped = xss.map(xs => xs.zipWithIndex).zipWithIndex
        zipped.flatMap(xs => xs._1.map(x => part1check(zipped, xs._2, x._2))).filter(x => x != 0).sum
    }

    def run(): Unit = {
        val input: List[List[Int]] = Source.fromFile("src/main/resources/day09.txt").mkString.split("\n").toList.map(_.toCharArray.map(x => x - 48).toList)
        println(part1(input))
    }
}

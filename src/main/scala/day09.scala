import scala.collection.mutable
import scala.io.Source

// Given a grid of integers 0-9, find basins (similar to water level problem).
class day09 {

    val input: List[List[Int]] = Source.fromFile("src/main/resources/day09.txt").mkString.split("\n").toList.map(_.toCharArray.map(x => x - 48).toList)
    var visited = Array.ofDim[Boolean](input.length, input(0).length)

    def isCorner(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Boolean = (i == 0 && j == 0) || (i == 0 && j == xss.head._1.length - 1) || (i == xss.length - 1 && j == 0) || (i == xss.length - 1 && j == xss.head._1.length - 1)
    def isCorner2(xss: List[List[Int]], i: Int, j: Int): Boolean = (i == 0 && j == 0) || (i == 0 && j == xss.head.length - 1) || (i == xss.length - 1 && j == 0) || (i == xss.length - 1 && j == xss.head.length - 1)

    def isEdge(xss: List[(List[(Int, Int)], Int)], i: Int, j: Int): Boolean = i == 0 || j == 0 || i == xss.length - 1 || j == xss.head._1.length - 1
    def isEdge2(xss: List[List[Int]], i: Int, j: Int): Boolean = i == 0 || j == 0 || i == xss.length - 1 || j == xss.head.length - 1

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

    def boolToInt(b: Boolean): Int = if (b) 1 else 0

    def checkNonStrictCorner(xss: List[List[Int]], i: Int, j: Int): Boolean =
        if (i == 0 && j == 0) xss.head.head < xss.head(1)   || xss.head.head < xss(1).head
        else if (i == 0)      xss.head(j) < xss.head(j - 1) || xss.head(j) < xss(1)(j)
        else if (j == 0)      xss(i).head < xss(i - 1).head || xss(i).head < xss(i)(1)
        else                  xss(i)(j) < xss(i - 1)(j)     || xss(i)(j) < xss(i)(j - 1)


    def checkNonStrictEdge(xss: List[List[Int]], i: Int, j: Int): Boolean = {
        if (i == 0)                   xss(i)(j) < xss(i + 1)(j) || xss(i)(j) < xss(i)(j + 1) || xss(i)(j) < xss(i)(j - 1)
        else if (j == 0)              xss(i)(j) < xss(i - 1)(j) || xss(i)(j) < xss(i + 1)(j) || xss(i)(j) < xss(i)(j + 1)
        else if (i == xss.length - 1) xss(i)(j) < xss(i - 1)(j) || xss(i)(j) < xss(i)(j + 1) || xss(i)(j) < xss(i)(j - 1)
        else                          xss(i)(j) < xss(i - 1)(j) || xss(i)(j) < xss(i + 1)(j) || xss(i)(j) < xss(i)(j - 1)
    }

    def checkNonStrictSquare(xss: List[List[Int]], i: Int, j: Int): Boolean =
        xss(i)(j) < xss(i + 1)(j) || xss(i)(j) < xss(i - 1)(j) || xss(i)(j) < xss(i)(j + 1) || xss(i)(j) < xss(i)(j - 1)

    def part2check(xss: List[List[Int]], i: Int, j: Int): Boolean = {
        if (xss(i)(j) == 9)          return false
        if (isCorner2(xss, i, j))    checkNonStrictCorner(xss, i, j)
        else if (isEdge2(xss, i, j)) checkNonStrictEdge(xss, i, j)
        else                         checkNonStrictSquare(xss, i, j)
    }

    def part1(xss: List[List[Int]]): Int = {
        val zipped = xss.map(xs => xs.zipWithIndex).zipWithIndex
        zipped.flatMap(xs => xs._1.map(x => part1check(zipped, xs._2, x._2))).filter(x => x != 0).sum
    }

    def canEnqueue(x: Int, y: Int, visited: Array[Array[Boolean]]): Boolean =
        x >= 0 && y >= 0 && x < visited.length && y < visited(0).length && !visited(x)(y)

    def dfs(xss: List[List[Int]], visited: Array[Array[Boolean]], stack: mutable.Stack[(Int, Int)]): Long = {
        var count = 0L
        while (stack.nonEmpty) {
            val pop = stack.pop()
            if (!visited(pop._1)(pop._2)) {
                visited(pop._1)(pop._2) = true
                if (part2check(xss, pop._1, pop._2)) {
                    count += 1
                    if (canEnqueue(pop._1 + 1, pop._2, visited)) stack.push((pop._1 + 1, pop._2))
                    if (canEnqueue(pop._1 - 1, pop._2, visited)) stack.push((pop._1 - 1, pop._2))
                    if (canEnqueue(pop._1, pop._2 + 1, visited)) stack.push((pop._1, pop._2 + 1))
                    if (canEnqueue(pop._1, pop._2 - 1, visited)) stack.push((pop._1, pop._2 - 1))
                }
            }
        }

        count
    }

    // Missing some edge case. Actually 96, 98 instead of 94 and 95.
    def part2(xss: List[List[Int]]): Long = {
        var first  = 0L
        var second = 0L
        var third  = 0L

        for (i <- xss.indices) {
            for (j <- xss(i).indices) {
                if (!visited(i)(j)) {
                    val newBasin = dfs(xss, visited, mutable.Stack[(Int, Int)]((i, j)))
                    if (newBasin > 0) {
                        if (newBasin > first) {
                            third = second
                            second = first
                            first = newBasin
                        } else if (newBasin > second) {
                            third = second
                            second = newBasin
                        } else third = Math.max(newBasin, third)
                    }
                }
            }
        }
        first * second * third
    }

    def run(): Unit = {
        println(part1(input))
        println(part2(input))
    }
}

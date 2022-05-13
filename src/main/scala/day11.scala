import scala.io.Source

class day11 {

    def isValid(i: Int, j: Int, m: Int, n: Int): Boolean = i >= 0 && j >= 0 && i < m && j < n

    def flash(input: Array[Array[Int]]): Array[Array[Int]] = {
        var xss = input
        val m = xss.length
        val n = xss.head.length
        for (i <- xss.indices) {
            for (j <- xss(0).indices) {
                if (xss(i)(j) > 9) {
                    for (x <- Array(-1, 0, 1))
                        for (y <- Array(-1, 0, 1))
                            if ((i != 0 && j != 0) && isValid(i + x, j + y, m, n))
                                xss(i + x)(j + y) += 1
                }
            }
        }
        xss
    }

    def pprint(xss: Array[Array[Int]]): Unit = {
        xss.foreach({
            x => x.map(y => print(y))
                println()
        })
    }

    def part1(input: Array[Array[Int]]): Long = {
        var count = 0L
        var xss = input
        val m = xss.length
        val n = xss.head.length
        for (_ <- Array.range(0, 100, 1)) {
            xss = xss.map(_.map(x => x + 1))

            while (xss.exists(_.exists(_ > 9)))
                for (i <- xss.indices)
                    for (j <- xss(0).indices)
                        if (xss(i)(j) > 9) {
                            for (x <- Array(-1, 0, 1))
                                for (y <- Array(-1, 0, 1))
                                    if (!(x == 0 && y == 0) && isValid(i + x, j + y, m, n) && xss(i + x)(j + y) != 0)
                                        xss(i + x)(j + y) = 1 + xss(i + x)(j + y)
                            xss(i)(j) = 0
                            count += 1
                        }
        }

        count
    }

    def part2(input: Array[Array[Int]]): Long = {
        var count = 0L
        var xss = input
        val m = xss.length
        val n = xss.head.length
        for (cc <- Array.range(0, 1000, 1)) {

            xss = xss.map(_.map(x => x + 1))
            count = 0
            while (xss.exists(_.exists(_ > 9)))
                for (i <- xss.indices)
                    for (j <- xss(0).indices)
                        if (xss(i)(j) > 9) {
                            for (x <- Array(-1, 0, 1))
                                for (y <- Array(-1, 0, 1))
                                    if (!(x == 0 && y == 0) && isValid(i + x, j + y, m, n) && xss(i + x)(j + y) != 0)
                                        xss(i + x)(j + y) = 1 + xss(i + x)(j + y)
                            xss(i)(j) = 0
                            count += 1
                        }

            if (count == input.map(_.length).sum) return cc + 1
        }
        -1
    }

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day11.txt").mkString.split("\n").map(_.toCharArray.map(x => x - 48))
        println(part1(input))
        println(part2(input))
    }
}

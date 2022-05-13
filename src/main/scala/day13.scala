import org.graalvm.compiler.api.replacements.Fold

import scala.io.Source

class day13 {


    def parseFold(s: String): (Char, Int) = {
        if (s startsWith "x") ('x', s.substring(2).toInt)
        else                  ('y', s.substring(2).toInt)
    }

    def parseCoordinates(s: String): (Int, Int) = {
        val t = s.split(",")
                (t(1).toInt, t(0).toInt)
    }

    def day13print(tuples: Array[(Int, Int)]): Unit = {
        val m = tuples.map(_._1).max + 1
        val n = tuples.map(_._2).max + 1

        for (i <- Range(0, m, 1)) {
            for (j <- Range(0, n, 1))
                if (tuples.contains((i, j))) print('#')
                else                         print('.')
            println()
        }
    }

    def foldHelper(xy: Int, axis: Int): Int =
        if (xy > axis) xy - 2 * (xy - axis)
        else           xy

    def fold(xs: Array[(Int, Int)], axis: (Char, Int)): Array[(Int, Int)] = {
        if (axis._1 == 'x') xs.map(x => (x._1, foldHelper(x._2, axis._2))).filter(_._2 >= 0).distinct
        else                xs.map(x => (foldHelper(x._1, axis._2), x._2)).filter(_._1 >= 0).distinct
    }

    def part1(xs: Array[(Int, Int)], axis: (Char, Int)): Int = fold(xs, axis).length

    def part2(coordinates: Array[(Int, Int)], folds: List[(Char, Int)]): Array[(Int, Int)] = {
        var arr = coordinates
        for (f <- folds) {
            arr = fold(arr, f)
        }
        arr
    }

    def run(): Unit = {
        val parts = Source.fromFile("src/main/resources/day13.txt").mkString.split("\n\n").toList
        val folds: List[(Char, Int)] = parts(1).split("\n").map(x => x.replace("fold along ", "")).map(parseFold).toList
        val coordinates: Array[(Int, Int)] = parts(0).split("\n").map(parseCoordinates)

        println(part1(coordinates, folds.head))
        day13print(part2(coordinates, folds))
    }
}

// FAGURZHE
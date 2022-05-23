import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.immutable.{HashMap, List}
import scala.collection.mutable.{HashMap}
import scala.io.Source

class day08 {

    val zero  = List(1, 2, 3, 5, 6, 7)
    val one   = List(3, 6)
    val two   = List(1, 3, 4, 5, 7)
    val three = List(1, 3, 4, 6, 7)
    val four  = List(2, 3, 4, 6)
    val five  = List(1, 2, 4, 6, 7)
    val six   = List(1, 2, 4, 5, 6, 7)
    val seven = List(1, 3, 6)
    val eight = List(1, 2, 3, 4, 5, 6, 7)
    val nine  = List(1, 2, 3, 4, 6, 7)

    val uniques       = collection.immutable.HashMap(2 -> one, 4 -> four, 3 -> seven, 7 -> eight)
    val nonUniques    = collection.immutable.HashMap(5 -> List(two, three, five), 6 -> List(zero, six, nine))
    val uniqueLengths = uniques.keys



    def part1(xss: List[(List[(String, Int)], List[(String, Int)])]): Int = xss.flatMap(_._2.map(_._2)).count(x => uniqueLengths.contains(x))

    // TODO: Create domains. Simple mapping won't work because of shuffling.
    def deduce(xs: (List[(String, Int)], List[(String, Int)])): Long = {
        var map = collection.mutable.HashMap(1 -> 'r', 2 -> 'r', 3 -> 'r', 4 -> 'r', 5 ->'r', 6 -> 'r', 7 -> 'r')
        xs._1.filter(x => uniqueLengths.contains(x._2)).filter(_._2 != 7).foreach(x => {
            for (u <- uniques(x._2).zip(x._1.toCharArray)) {
                if (map(u._1) == 'r')
                    map.update(u._1, u._2)
            }
        })
        0
    }

    def part2(xss: List[(List[(String, Int)], List[(String, Int)])]): Long = xss.map(deduce).sum

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day08example.txt")
            .mkString.split("\n")
            .map(_.split("\\|"))
            .map(x => (x(0).trim.split(" ").toList.map(x => (x, x.length)), x(1).trim.split(" ").toList.map(x => (x, x.length))))
            .toList

//        println(part1(input))
        println(part2(input))
    }
}

/**
 * 0: 6
 * 1: 2 unique
 * 2: 5
 * 3: 5
 * 4: 4 unique
 * 5: 5
 * 6: 6
 * 7: 3 unique
 * 8: 7 unique
 * 9: 6
 */
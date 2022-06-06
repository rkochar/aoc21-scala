import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.immutable.{HashMap, List}
import scala.collection.mutable.{HashMap}
import scala.io.Source

class day08 {

    val uniqueLengthMap: Map[Int, Int] = collection.immutable.HashMap(2 -> 1, 4 -> 4, 3 -> 7, 7 -> 8)
    val uniqueLengths: Iterable[Int]   = uniqueLengthMap.keys

    def part1(xss: List[(List[(String, Int)], List[(String, Int)])]): Int = xss.flatMap(_._2.map(_._2)).count(x => uniqueLengths.contains(x))

    def find(xss: (List[(List[Char], Int)], List[(List[Char], Int)])): Long = {
        val map: Array[List[Char]] = Array.ofDim(10)
        val uniques    = xss._1.filter(x => uniqueLengths.contains(x._2))
        val nonUniques = xss._1.filterNot(x => uniqueLengths.contains(x._2))
        for (u <- uniques)
            map(uniqueLengthMap(u._2)) = u._1
        val six  = nonUniques.filter(_._2 == 6).map(_._1)
        val five = nonUniques.filter(_._2 == 5).map(_._1)

        map(6) = six.filter(x => oneCommon(x, map(1))).head
        map(0) = six.filterNot(x => x == map(6)).filter(x => oneDiff(x, map(4))).head
        map(9) = six.filterNot(x => x == map(6)).filterNot(x => x == map(0)).head

        map(5) = five.filter(x => map(6).intersect(x).length == 5).head
        map(3) = five.filterNot(x => x == map(5)).filter(x => map(9).intersect(x).length == 5).head
        map(2) = five.filterNot(x => x == map(5)).filterNot(x => x == map(3)).head

        xss._2.map(x => findWord(x, map)).zipWithIndex.foldRight(0l)((x, acc) => acc + x._1 * Math.pow(10, 3 - x._2).toLong)
    }

    def equalLetters(xs: List[Char], ys: List[Char]): Boolean = xs.intersect(ys).length == xs.length && xs.length == ys.length

    def findWord(ys: (List[Char], Int), hashMap: Array[List[Char]]): Long =
        if (uniqueLengths.contains(ys._2)) uniqueLengthMap(ys._2)
        else                               hashMap.indexOf(hashMap.filter(x => equalLetters(x, ys._1)).head)

    def oneCommon(x: List[Char], y: List[Char]): Boolean = x.intersect(y).length == 1

    def oneDiff(x: List[Char], y: List[Char]): Boolean = x.intersect(y).length == Math.min(x.length, y.length) - 1

    def part2(xss: List[(List[(String, Int)], List[(String, Int)])]): Long =
        xss.map(xs => (xs._1.map(x => (x._1.toCharArray.toList, x._2)), xs._2.map(x => (x._1.toCharArray.toList, x._2)))).map(find).sum

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day08.txt")
            .mkString.split("\n")
            .map(_.split("\\|"))
            .map(x => (x(0).trim.split(" ").toList.map(x => (x, x.length)), x(1).trim.split(" ").toList.map(x => (x, x.length))))
            .toList

        println(part1(input))
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

/**
 * 1, 6
 * 4, 0
 *
 * 5 from 6
 * 3 from 9
 */
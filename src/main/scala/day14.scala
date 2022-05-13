import scala.collection.mutable
import scala.io.Source

class day14 {

    var hashMap = new mutable.HashMap[String, (String, Int)]()

    def generateRules(word: List[Char]): List[String] = word match {
        case x1 :: x2 :: Nil => x1.toString + x2.toString :: Nil
        case x1 :: x2 :: xs  => x1.toString + x2.toString :: generateRules(x2 :: xs)
    }

    def polymerize(word: String): String = generateRules(word.toCharArray.toList).map(x => hashMap(x)).mkString

    def parseRule(rule: String): (String, String) = {
        val t = rule.split(" -> ")
        (t(0), t(0).charAt(0) + t(1) + t(0).charAt(1))
    }

    def part1Helper(word: String, i: Int, count: Int): String =
        if (i < count) part1Helper(polymerize(word), i + 1, count)
        else word

    def part1(startWord: String): Int = {
        val word = part1Helper(startWord, 0, 7)
//        val t = part1Helper(startWord, hashMap, 0, 10).toCharArray.map(x => (x, 1)).groupBy(_._1).map(x => x._2.length)
        0
    }

    def run(): Unit = {
        val parts = Source.fromFile("src/main/resources/day14example.txt").mkString.split("\n\n").toList
        val word = parts(0).strip()
        val rules = parts(1).split("\n").map(parseRule).toList
        for (r <- rules)
            hashMap = hashMap + (r._1 -> (r._2, 0))

        println(part1(word))
//        println(part2(input))
    }
}

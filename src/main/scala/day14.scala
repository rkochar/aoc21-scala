import scala.collection.mutable
import scala.io.Source

class day14 {

    var hashMap = new mutable.HashMap[String, List[String]]()
    var counter = new mutable.HashMap[String, Long]()
    var letters = new mutable.HashMap[Char, Long]()

    def generateRules(word: List[Char]): List[String] = word match {
        case x1 :: x2 :: Nil => x1.toString + x2.toString :: Nil
        case x1 :: x2 :: xs  => x1.toString + x2.toString :: generateRules(x2 :: xs)
    }

    def iterateRules(): Unit = {
        var newCounter = new mutable.HashMap[String, Long]()
        for (c <- counter) {
            if (c._2 > 0) {
                letters(hashMap(c._1)(0).charAt(0)) += 1l
                letters(hashMap(c._1)(0).charAt(1)) += 2l
                letters(hashMap(c._1)(1).charAt(1)) += 1l
                for (hm <- hashMap(c._1))
                    newCounter.get(hm) match {
                        case Some(_) => newCounter(hm) += 1l
                        case None    => newCounter = newCounter + (hm -> 1l)
                    }
            }
        }
        // Reset counter with next word so that rules are not repeated.
        for (c <- counter.keys)
            newCounter.get(c) match {
                case Some(_) => counter(c) = newCounter(c)
                case None    => counter(c) = 0l
            }

    }

    def parseRule(rule: String): (String, List[String]) = {
        val t = rule.split(" -> ")
        (t(0), List(t(0).charAt(0) + t(1), t(1) + t(0).charAt(1)))
    }

    def calculateLetters(word: String): Long = {
        letters(word.charAt(0)) += 1l
        letters(word.charAt(word.length - 1)) += 1l
        for (l <- letters.keys)
            letters(l) /= 2
        println(letters)
        letters.values.max - letters.values.min
    }

    def part1(i: Int, count: Int, word: String): Long =
        if (i < count) {
            iterateRules()
            part1(i + 1, count, word)
        }
        else calculateLetters(word)

    def run(): Unit = {
        val parts = Source.fromFile("src/main/resources/day14example.txt").mkString.split("\n\n").toList
        val rules = parts(1).split("\n").map(parseRule).toList
        for (r <- rules.flatMap(_._2).distinct)
            letters = letters + (r.charAt(0) -> 0L)
        for (r <- rules)
            hashMap = hashMap + (r._1 -> r._2)
        for (x <- (hashMap.values.toList.flatten ++ hashMap.keys).distinct)
            counter = counter + (x -> 0)
        for (w <- generateRules(parts.head.strip().toCharArray.toList))
            counter(w) += 1

        println(part1(0, 10, parts.head.strip()))
//        println(part2(input))
    }
}

import scala.collection.mutable.ListBuffer
import scala.io.Source

class day10 {

    val closingBrackets = List(')', '}', ']', '>')

    def findValuePart1(c: Char): Int = {
        if (c == ')')      3
        else if (c == ']') 57
        else if (c == '}') 1197
        else               25137
    }

    def findValuePart2(c: Char): Int = {
        if (c == '(')      1
        else if (c == '[') 2
        else if (c == '{') 3
        else               4
    }

    def reduceToClosingBracket(ch: List[Char]): Char = {
        if  (closingBrackets.contains(ch.head)) ch.head
        else                                    reduceToClosingBracket(ch.tail)
    }

    def completeBrackets(chars: List[Char]): Long = {
        var count = 0L
        for (ch <- chars) {
            count = (count * 5) + findValuePart2(ch)
        }
        count
    }

    def part1(xs: List[List[Char]]): Int = xs.map(reduceToClosingBracket).map(findValuePart1).sum

    def part2(xs: List[List[Char]]): Long = {
        val t = xs.map(_.reverse).map(completeBrackets).sorted
        // Fold needs AnyVal for some reason.
//        val t = xs.map(_.reverse).map(_.map(findValuePart2)).map(_.fold(0L)((x, y) => x * 5l + y)).sorted
        t(xs.length / 2)
    }

    def stripPairs(l: String): String = {
        var line = l
        if (line.contains("()"))      {
            line = line.replace("()", "")
            stripPairs(line)
        }
        else if (line.contains("{}")) {
            line = line.replace("{}", "")
            stripPairs(line)
        }
        else if (line.contains("[]")) {
            line = line.replace("[]", "")
            stripPairs(line.replace("[]", ""))
        }
        else if (line.contains("<>")) {
            line = line.replace("<>", "")
            stripPairs(line.replace("<>", ""))
        }
        else                          l
    }

//    def stripPairs(line: String): String = {
//        if (line.contains("()"))      stripPairs(line.replace("()", ""))
//        else if (line.contains("{}")) stripPairs(line.replace("{}", ""))
//        else if (line.contains("[]")) stripPairs(line.replace("[]", ""))
//        else if (line.contains("<>")) stripPairs(line.replace("<>", ""))
//        else                          line
//    }

    def checkCorrupt(line: String): Boolean = line.contains(']') || line.contains('}') || line.contains(')') || line.contains('>')

    def seperateLines(lines: List[String]): (List[String], List[String]) = {
        var corrupted: List[String] = List()
        var incomplete: List[String] = List()
        val linesIterator = lines.iterator
        while (linesIterator.hasNext) {
            val line = stripPairs(linesIterator.next())
            if (checkCorrupt(line)) corrupted = corrupted :+ line
            else                    incomplete = incomplete :+ line
        }
        (corrupted, incomplete)
    }


    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day10.txt").mkString.split("\n").toList
        val (corrupted, incomplete) = seperateLines(input)
        println(part1(corrupted.map(_.toCharArray.toList)))
        println(part2(incomplete.map(_.toCharArray.toList)))
    }
}

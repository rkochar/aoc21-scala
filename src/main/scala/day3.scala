import scala.io.Source

class day3 {

    // Doesn't always work. 3847100
    def part1(xs: List[String]): Long = {
        val count = xs.length
        val frequencies = frequencyMap(xs).map(x => {
            if (x._2 > count/2) "1"
            else "0"
        }).toList.reverse

        val gamma = frequencies.zipWithIndex.map(x => x._1.toInt * (Math.pow(2, x._2))).sum.toInt
        val epsilon = frequencies.map(x => {
            if (x == "0") "1"
            else "0"
        }).zipWithIndex.map(x => x._1.toInt * (Math.pow(2, x._2))).sum.toInt

        gamma * epsilon
    }

    def makeTwo(x: String): Int = x match {
        case "1" => 2
        case "0" => 0
    }

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day3.txt").mkString.linesIterator.toList
        println("Part 1 is: " + part1(input))
        println("Part 2 is: " + part2(input))
    }

    def part2(xs: List[String]) = {
        binaryToDecimal(part2oxygen(xs, 0)) * binaryToDecimal(part2co2(xs, 0))
    }

    def part2oxygen(xs: List[String], index: Int): String = {
        val count = xs.length
        if (count == 1) return xs.head

        if (frequencyMap(xs)(index) * 2 >= count) {
            part2oxygen(xs.filter(x => (x.charAt(index) == '1')), index + 1)
        } else {
            part2oxygen(xs.filter(x => (x.charAt(index) == '0')), index + 1)
        }
    }

    def part2co2(xs: List[String], index: Int): String = {
        val count = xs.length
        if (count == 1) return xs.head

        if (frequencyMap(xs)(index) * 2 >= count) {
            part2co2(xs.filter(x => (x.charAt(index) == '0')), index + 1)
        } else {
            part2co2(xs.filter(x => (x.charAt(index) == '1')), index + 1)
        }
    }

    def binaryToDecimal(xs: String): Int = xs.toCharArray.reverse.zipWithIndex.map(x => (x._1.toString.toInt * Math.pow(2, x._2))).sum.toInt

    def frequencyMap(xs:List[String]): Map[Int, Int] = xs.flatMap(x => x.toCharArray.zipWithIndex).filter(x => x._1 == '1').groupBy(_._2).map(x => (x._1, x._2.foldLeft(0)((y, _) => y + 1)))

}

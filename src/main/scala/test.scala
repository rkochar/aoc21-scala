import scala.io.Source

class test {

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/grades.txt").mkString.split("\n").map(_.toFloat).sum
        println(input)
        println("cs : " + (5 * input / 145))
        val rob = (8.5 + 7 + 8 + 8 + 8 + 6) // (4 * 8.5 + 2 * 7 + 15 * 8 + 3 * 6) / 24
        println("all: " + (5 * (input + rob) / 169))
    }
}

//6.0
//8.5
//7.0
//8.0
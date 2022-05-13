import scala.io.Source

class test {

    def count(yActual: List[Int], yPred: List[Int]): Double = yActual.zip(yPred).count(x => x._1 == x._2)

    def run(): Unit = {
        val yActual: List[Int] = Source.fromFile("src/main/resources/solution.txt").mkString.split("\n").map(_.toInt).toList
        val yPredicted = Source.fromFile("src/main/resources/Group_44_classes.txt").mkString.split(",").map(_.toInt).toList
        println((count(yActual, yPredicted) / yActual.length): Double)
    }
}

import scala.io.Source

/**
 * Find smallest cost path from top left to bottom right.
 */
class day15 {

    // Should not work but it does.
    // Assumption of always going right or down. Can do O(n^3) dp or Dijkstra to always work.
    // Edge case:
    // 1 10 1 1  1
    // 1 1  1 10 1
    def part1(input: Array[Array[Long]]): Long = {
        val xss = Array.ofDim[Long](input.length, input(0).length)
        for (i <- 1 until xss(0).length)
            xss(0)(i) = input(0)(i) + xss(0)(i - 1)
        for (i <- 1 until input.length)
            for (j <- input(0).indices) {
                val top =  if (i - 1 >= 0) xss(i - 1)(j) else Int.MaxValue
                val left = if (j - 1 >= 0) xss(i)(j - 1) else Int.MaxValue
                xss(i)(j) = input(i)(j) + Math.min(left, top)
            }

        xss.last.last
    }

    def mod(i: Long): Long = if (i <= 9) i else mod(i - 9)

    def part1dijkstra(xss: Array[Array[Long]]): Long = {
        val nodes = Array.ofDim[NodeDay15](xss.length, xss(0).length) //.map(_.toList).toList
        for (i <- xss.indices)
            for (j <- xss(0).indices)
                nodes(i)(j) = new NodeDay15((i, j), xss(i)(j))
        nodes.head.head.distance = 0l
        nodes.head.head.visited  = true
        nodes.head(1).distance = 0 //xss(0)(1)
        nodes(1).head.distance = 0 //xss(1)(0)

        while (!nodes.flatten.forall(_.visited)) {
            val minNode = nodes.flatten.filter(!_.visited).minBy(_.distance)
            minNode.visited = true
            if (minNode.id._1 > 0 && !nodes(minNode.id._1 - 1)(minNode.id._2).visited)
                updateNode(nodes(minNode.id._1 - 1)(minNode.id._2), minNode)
            if (minNode.id._2 > 0 && !nodes(minNode.id._1)(minNode.id._2 - 1).visited)
                updateNode(nodes(minNode.id._1)(minNode.id._2 - 1), minNode)
            if (minNode.id._1 < xss.length - 1 && !nodes(minNode.id._1 + 1)(minNode.id._2).visited)
                updateNode(nodes(minNode.id._1 + 1)(minNode.id._2), minNode)
            if (minNode.id._2 < xss(0).length - 1 && !nodes(minNode.id._1)(minNode.id._2 + 1).visited)
                updateNode(nodes(minNode.id._1)(minNode.id._2 + 1), minNode)
        }

        nodes.last.last.distance + nodes.last.last.cost
    }

    def updateNode(otherNode: NodeDay15, currentNode: NodeDay15): Unit =
        otherNode.distance = Math.min(otherNode.distance, currentNode.cost + currentNode.distance)

    def part2(input: Array[Array[Long]]): Long = {
        val xss = Array.ofDim[Long](input.length * 5, input(0).length * 5)
        for (i <- 0 until 5)
            for (j <- 0 until 5)
                for (x <- input.indices)
                    for (y <- input(0).indices)
                        xss(i * input.length + x)(j * input(0).length + y) = mod(input(x)(y) + i + j)

        part1dijkstra(xss)
    }

    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day15example.txt").mkString.split("\n").map(_.toCharArray.map(x => x - 48l))
        println(part1dijkstra(input))
        println(part2(input))
    }

}

class NodeDay15(xy: (Int, Int), originalCost: Long) {
    var id: (Int, Int) = xy
    var cost                = originalCost
    var distance: Long      = Long.MaxValue
    //    var previous: String    = "None"
    var visited: Boolean    = false

    def getId(): (Int, Int) = id
}
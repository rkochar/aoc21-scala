import scala.io.Source

class day12 {
    // TOOD: NQuuen style ordered recursion while marking and unmarking caves.
    def run(): Unit = {
        val input = Source.fromFile("src/main/resources/day12example.txt").mkString.split("\n")
        val nodeNames = input.flatMap(_.split('-')).distinct
        val l = nodeNames.length
        var nodes = new Array[NodeDay12](l + 2)
        nodes(1) = new NodeDay12(1, "start", false)
        nodes(l + 1) = new NodeDay12(l + 1, "end", false)
        for (i <- Range(2, l, 1)) {
            val name: String = nodeNames(i - 1)
            val small: Boolean = name.toCharArray.forall(_ >= 97)
            if (name != "start" || name != "end") nodes(i) = new NodeDay12(i, name, small)
        }
        for (edge <- input) {
            val e = edge.split('-')
            val from = nodeNames.indexOf(e(0))
            val to = nodeNames.indexOf(e(1))
            nodes(from).insertEdge(to)
        }
        println(nodes)
    }
}

class NodeDay12(newId: Int, newName: String, small: Boolean) {
    val id      = newId
    val name    = newName
    val isSmall = small

    var visited: Boolean  = false
    var edges: Array[Int] = Array.empty

    def insertEdge(toId: Int): Unit = this.edges = this.edges :+ toId

    def visit(): Unit = if (this.isSmall) visited = true
}

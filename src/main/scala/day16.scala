import scala.collection.immutable.HashMap
import scala.io.Source

class day16 {

    sealed class Packet()
        case class NoPacket()                                                           extends Packet
        case class SubPacket(version: Int, typeId: Int, message: Long)                  extends Packet
        case class OperatorPacket(version: Int, typeId: Int, lengthId: Int,
                                  bitValue: Long, listPacket: List[Packet])             extends Packet

    val hexMap: HashMap[Char, String] = collection.immutable.HashMap(
        '0' -> "0000",
        '1' -> "0001",
        '2' -> "0010",
        '3' -> "0011",
        '4' -> "0100",
        '5' -> "0101",
        '6' -> "0110",
        '7' -> "0111",
        '8' -> "1000",
        '9' -> "1001",
        'A' -> "1010",
        'B' -> "1011",
        'C' -> "1100",
        'D' -> "1101",
        'E' -> "1110",
        'F' -> "1111")

    def binaryToDecimal(xs: String): Long = xs.toCharArray.reverse.zipWithIndex.map(x => (x._1.toString.toInt * Math.pow(2, x._2))).sum.toLong

    def interpSimplePacket(packet: String): String =
        if (packet.head == '0') packet.substring(1, 5)
        else                    packet.substring(1, 5) + parseSimplePacket(packet.substring(5))

    def simplePacketHelper(packet: String): (String, String) =
        if (packet.head == '0') (packet.substring(1, 5), packet.substring(5))
        else                    {
            val rest: (String, String) = simplePacketHelper(packet.substring(5))
            (packet.substring(1, 5) + rest._1, rest._2)
        }

    def parseSimplePacket(packet: String): (Packet, String) = {
        val thisPacket = simplePacketHelper(packet.substring(6))
        (SubPacket(binaryToDecimal(packet.substring(0, 3)).toInt, binaryToDecimal(packet.substring(3, 6)).toInt, binaryToDecimal(thisPacket._1)), thisPacket._2)
    }

    def parseNPackets(n: Long, string: String): (List[Packet], String) = {
        var count                    = n
        var newString                = string
        var listPacket: List[Packet] = List()
        while (count > 0) {
            val parsed = parse(newString)
            count -= 1
            newString = parsed._2
            listPacket = listPacket :+ parsed._1
        }
        (listPacket, newString)
    }

    def parseNLengthPackets(n: Long, string: String): (List[Packet], String) = {
        var count                    = n
        var newString                = string
        var listPacket: List[Packet] = List()
        while (count > 0) {
            val parsed = parse(newString)
            count -= newString.length - parsed._2.length
            newString = parsed._2
            listPacket = listPacket :+ parsed._1
        }
        (listPacket, newString)
    }

    def parseOperatorPacket(string: String): (OperatorPacket, String) = {
        val lengthId = string.charAt(6).toInt - 48
        val (bitValue, newString) = if (lengthId == 0) (binaryToDecimal(string.substring(7, 22)), string.substring(22))
                                    else               (binaryToDecimal(string.substring(7, 18)), string.substring(18))
        val (packets, remain) = if (lengthId == 0) parseNLengthPackets(bitValue, newString)
                                else               parseNPackets(bitValue, newString)
        (OperatorPacket(binaryToDecimal(string.substring(0, 3)).toInt,
                        binaryToDecimal(string.substring(3, 6)).toInt,
                        lengthId, bitValue, packets),
            remain)
    }

    def parse(packet: String): (Packet, String) =
        if (!packet.contains('1'))                (NoPacket(), "")
        else if (packet.substring(3, 6) == "100") parseSimplePacket(packet)
        else                                      parseOperatorPacket(packet)

    def findVersionSum(packet: Packet): Long = packet match {
        case NoPacket()                                          => 0L
        case SubPacket(version: Int, _, _)                       => version
        case OperatorPacket(version: Int, _, _, _,
                            listPackets: List[Packet])           => version + listPackets.map(findVersionSum).sum
    }

    def greaterThan(xs: List[Long]): Long = if (xs.head > xs.tail.head) 1L else 0L

    def lesserThan(xs: List[Long]): Long = greaterThan(xs.reverse)

    def equalTo(xs: List[Long]): Long = if (xs.head == xs.tail.head) 1L else 0L

    def solveOperatorPacket(operatorPacket: OperatorPacket): Long = operatorPacket.typeId match {
        case 0 => operatorPacket.listPacket.map(solvePacket).sum
        case 1 => operatorPacket.listPacket.map(solvePacket).product
        case 2 => operatorPacket.listPacket.map(solvePacket).min
        case 3 => operatorPacket.listPacket.map(solvePacket).max
        case 5 => greaterThan(operatorPacket.listPacket.map(solvePacket))
        case 6 => lesserThan (operatorPacket.listPacket.map(solvePacket))
        case 7 => equalTo    (operatorPacket.listPacket.map(solvePacket))
    }

    def solvePacket(packet: Packet): Long = packet match {
        case NoPacket()                                            => 0L
        case SubPacket(_, _, message: Long)                        => message
        case op@OperatorPacket(_, _, _, _, _)                      => solveOperatorPacket(op)
    }

    def part1(input: Packet): Long = findVersionSum(input)

    def part2(input: Packet): Long = solvePacket(input)

    def run(): Unit = {
        val packets = Source.fromFile("src/main/resources/day16.txt").mkString.toCharArray.map(hexMap).fold("")((x: String, y: String) => x ++ y)
        val parsedPackets = parse(packets)
        println(part1(parsedPackets._1))
        println(part2(parsedPackets._1))
    }
}

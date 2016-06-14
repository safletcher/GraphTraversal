import scala.collection.mutable.ListBuffer
import scala.io.Source

case class ChildNode (Transport:String, Child:String)

class Graph {
    private val GraphMap = scala.collection.mutable.HashMap[String, List[ChildNode]]()
    private val allPaths :ListBuffer[ListBuffer[(Option[String],String)]] = ListBuffer.empty

    def clearAllPath = {allPaths.clear() }

    def addNode( Parent:String, Transport:String , Child:String) = {
        var tempNode: List[ChildNode] = Nil

        tempNode = GraphMap.getOrElse(Parent,Nil)
        tempNode = new ChildNode(Transport, Child) :: tempNode
        GraphMap(Parent) = tempNode

        if (!GraphMap.contains(Child)) {
            GraphMap(Child) = Nil
        }
    }

    def findPaths(Start: String, End:String,transport:Option[String] = None, visited : ListBuffer[(Option[String],String)] = ListBuffer.empty): ListBuffer[String] = {

        val tup = (transport, Start)
        visited += tup

        if (Start.equals(End)) {
            allPaths += visited
            return ListBuffer.empty
        }

        for ((trans,element) <- GraphMap(Start).map(c=> (Some(c.Transport), c.Child))) {
            if (!visited.contains(element)) {
                val newVisit = visited.clone();
                findPaths(element, End,trans, newVisit)
            }
        }

        return ListBuffer.empty
    }

    def getLinks = GraphMap
    def getAllPath = allPaths
    def getShortest = allPaths.sortBy(_.length).take(1)
}

object GraphTraversal {

    def main(args: Array[String]): Unit = {
        val SampleGraph = new Graph()

        //Pass file on command line
        loadGraphFile(args(0),SampleGraph)

        while(true) {

            val input = readLine("Please Enter commands : ")
            val parsed_input = input.split(" ").map(str=> str.trim)

            if(parsed_input(0).toLowerCase.equals("help")) {
                println("Enter [hosts] to see hosts")
                println("Enter [links] to see linkage")
                println("To see best path enter for example : path A -> B")
                println("to add new linkage : load E sftp F")
                println("Enter [exit] to quit ")

            } else if(parsed_input(0).toLowerCase.equals("exit")) {
                return Unit
            } else if(parsed_input(0).toLowerCase.equals("path")) {
                val Array(command:String, start:String, arrow:String, end:String) =
                    input.split(" ").map(x=> x.trim)

                SampleGraph.clearAllPath
                println(s"SampleGraph.findPaths(${start},${end})")
                SampleGraph.findPaths(start,end)
                println(SampleGraph.getShortest)
            } else if (parsed_input(0).toLowerCase.equals("hosts") ) {
                println(SampleGraph.getLinks.map(x=> x._1))
            } else if (parsed_input(0).toLowerCase.equals("links") ) {
                println(SampleGraph.getLinks)

            } else if (parsed_input(0).toLowerCase.equals("load") ) {
                val Array(command:String, host1:String, desc:String, host2:String) =
                    input.split(" ").map(x=> x.trim)
                println(s"Loading new entries : ${command} ${host1} ${desc} ${host2}")
                SampleGraph.addNode(host1, desc, host2)
            }
        }

        return Unit
    }

    def loadGraphFile(GraphFilePath:String, SampleGraph:Graph ) = {
        for(line <- Source.fromFile(GraphFilePath).getLines()) {
            val Array(host1:String, desc:String, host2:String) =  line.split('|').map(x=> x.trim)
            SampleGraph.addNode(host1, desc, host2)
        }

        println("Graph Loaded : ")
        println(SampleGraph.getLinks)
    }
}

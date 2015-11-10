package CSSR.parse

class ParseTree {
  var tree:ParseTreeNode

  def addString (str:String) :ParseTree = {
    tree = addString(str, new ParseTreeNode())
    this
  }

  protected def addString (str:String, tree:ParseTreeNode): ParseTreeNode = {
    if (str.isEmpty) {
      tree
    } else {
      val nextNode = new ParseTreeNode(str.head)
      tree.children += nextNode
      addString(str.tail, nextNode)
    }
  }

//  def getDepth (int: Int): List[String] = {
//    getDepth(int, tree.children.toList, List(tree.value))
//  }

  protected def getDepth (int: Int, nodes:List[ParseTreeNode], strings:List[String]):List[String] = {
    if (int <= 0) strings else {
      var nextStrings = Array()
      for (node <- nodes){
        nextStrings ++= strings.map(s => node.value + s)
      }
      val nextNodes = nodes.reduce(List())(acc, node => )
      getDepth(int - 1, nextNodes, nextStrings)
    }
  }
}

class ParseTreeNode (val value:Char = "") {
  var children:Set[ParseTreeNode] = Set()

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParseTreeNode]

  override def equals(other: Any): Boolean = other match {
    case that: ParseTreeNode =>
      (that canEqual this) &&
        value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

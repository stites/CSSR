package CSSR.states

import java.lang.ArrayIndexOutOfBoundsException

class Translations {
  var parents:Array[TransNode] = Array()

  var translationNodes:Array[TransNode] = Array()

  /**
    * setTrans is setting up a linkedList which is better served with a hashmap
    * @param transStateIdx
    * @param string
    * @param parentStateIdx
    * @return
    */
  def setTrans(transStateIdx:Int, string:String, parentStateIdx:Int) :Unit = {
      //create a new TransNode and put it at the front of the list
      if ((transStateIdx >= 0) && (transStateIdx < translationNodes.size)) {

        //set array which finds by transition state
        var temp1 = new TransNode(string, parentStateIdx, null)

        var temp2 = translationNodes(transStateIdx)
        translationNodes(transStateIdx) = temp1
        temp1.nextNode = temp2

        var temp3 = new TransNode(string, transStateIdx, null)

        var temp4 = parents(parentStateIdx)
        translationNodes(parentStateIdx) = temp3
        temp3.nextNode = temp4
      }
  }


  /**
    * Remove Strings and State removes elements from the trans tables
    * @param removalState state to remove
    * @throws ArrayIndexOutOfBoundsException
    * @return
    */

  def RemoveStringsAndState(removalState:Int):Array[TransNode] = {
    //first node which is an element of a bad state
    var removeTemp = Option(parents(removalState))

    while (removeTemp.nonEmpty) {
      //transition entry containing that node
      var transStateIdx = removeTemp.get.stateIdx
      var transState = if (transStateIdx > translationNodes.length - 1) translationNodes.last else translationNodes(transStateIdx)
      if (transStateIdx == 0) {
        translationNodes(0) = null
      } else {
        var foundState = translationNodes.find(n => n.string == removeTemp.get.string)
        if (foundState.nonEmpty) {
          //remove node while keeping linked list together
          delete foundState;
        }
      }
      //move on to next node to remove all nodes
      removeTemp2 = removeTemp;
      removeTemp = removeTemp->nextPtr;
      //get rid of entry in parent Array
      delete removeTemp2;
    }
    //get rid of nodes which transition to bad state
    removeTemp = m_transArray[removalState];
    while (removeTemp) {
      removeTemp2 = removeTemp;
      removeTemp = removeTemp->nextPtr;
      removeTemp2->stringPtr = NULL;
      delete removeTemp2;
    }

    parents
  }


  /* unknown
  int TransTable::getCounts(int state) {
    return m_transCounts[state];
  }
  */


  def whichStrings(state:Int):TransNode = translationNodes(state)
}

class TransNode (val string:String, val stateIdx:Int, var nextNode:TransNode) {

}

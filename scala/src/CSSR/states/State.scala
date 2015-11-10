package CSSR.states

import scala.collection.immutable.TreeMap

class State {
  var freqCounts:Map[String, Int] = TreeMap.empty[String, Int]

 /**
   * Adds a new string to the state
   * @param string string to add to the frequency counts
   * @return void
   */
  def insert (string:String): Unit = {
    freqCounts += (string -> (freqCounts.getOrElse[Int](string, 0) + 1))
  }

  /**
    * calculates the frequency of occurrence of each element given state - P(element|state)
    * @param string observed string?
    * @return void
    */
  def calculateCurrentDistance (string: String): Unit = {
    freqCounts.keys.count( x => x.length == string.tail.length )
    return

    /* from States.cpp
    while (stringElem != NULL) {
      for (int i = 0; i < m_distributionSize; i++) {
        m_occurenceCount += stringElem->m_counts[i];
        m_currentDist[i] += (double) stringElem->m_counts[i];
      }
      stringElem = stringElem->m_nextPtr;
    }
    //divide all counts by total number of strings in state
    if (m_occurenceCount != 0) {
      for (int k = 0; k < m_distributionSize; k++) {
        m_currentDist[k] = ((m_currentDist[k]) / ((double) m_occurenceCount));
      }
    }
    */
  }

}

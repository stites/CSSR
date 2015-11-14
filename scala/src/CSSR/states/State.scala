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


  ///////////////////////////////////////////////////////////////////////////
  // Function: AllStates::Compare
  // Purpose: calls the KS statistic
  // In Params: the state to compare,the new distribution, the new data points
  // Out Params: significance level
  // In/Out Params: none
  // Pre- Cond: distributions have been calculated and set into arrays
  // Post-Cond: sig level is  known
  //////////////////////////////////////////////////////////////////////////
  /**
    * Calls the KS Statistic
    * @param newDistribution
    * @param count
    * @return
    */
  def compare(newDistribution:Array[Double], count:Int) :Double = {
    runTest(this.getCurrentDistance(), this.getCount(), newDistribution, count, m_distSize);
  }


  ///////////////////////////////////////////////////////////////////////////
  // Function: AllStates::Compare
  // Purpose: calls the KS statistic
  // In Params: the states to compare
  // Out Params: significance level
  // In/Out Params: none
  // Pre- Cond: distributions have been calculated and set into arrays
  // Post-Cond: sig level is  known
  //////////////////////////////////////////////////////////////////////////
  double AllStates::Compare(int k, int j) {
    return m_test->RunTest(m_StateArray[k]->getCurrentDist(),
      m_StateArray[k]->getCount(),
      m_StateArray[j]->getCurrentDist(),
      m_StateArray[j]->getCount(), m_distSize);
  }


  ///////////////////////////////////////////////////////////////////////////
  // Function: AllStates::Compare
  // Purpose: calls the KS statistic
  // In Params: the state to compare,the new distribution, the new data points
  // Out Params: significance level
  // In/Out Params: none
  // Pre- Cond: distributions have been calculated and set into arrays
  // Post-Cond: sig level is  known
  //////////////////////////////////////////////////////////////////////////
  double AllStates::Compare(int k, double newDist[], int count) {
    return m_test->RunTest(m_StateArray[k]->getCurrentDist(),
      m_StateArray[k]->getCount(), newDist, count,
      m_distSize);
  }
}

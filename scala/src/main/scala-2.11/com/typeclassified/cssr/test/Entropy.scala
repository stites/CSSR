package com.typeclassified.cssr.test

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.cssr.{EquivalenceClass, CausalState}
import com.typeclassified.cssr.parse.{ParseAlphabet, ParseTree}

object Entropy {

  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine
    *
    * @param parseTree
    * @param parseAlphabet
    */
  def calculateRelativeEntropy (parseTree:ParseTree, S:Array[CausalState], alphabet: ParseAlphabet): Double = {
    // We can't begin a substring of length maxLength at the last (maxLength-1)
    // positions in the data string
    val gArray = parseTree.getDepth(parseTree.maxLength)
    val stringProbs = DenseVector.zeros[Double](gArray.length)

    calcStringProbs(gArray, parseTree.maxLength, S, alphabet, stringProbs)

    var relEntropy:Double = 0
    var logRatio:Double = 0

    for ((history, i) <- gArray.view.zipWithIndex) {
      val occurrence:Double = sum(history.frequency)
      val dataProb:Double = occurrence / parseTree.adjustedDataSize

      if (dataProb > 0) {
        logRatio = math.log(dataProb) - math.log(stringProbs(i))
        logRatio *= dataProb
        relEntropy += logRatio
      }
    }
    relEntropy = relEntropy/math.log(2)

    return if (relEntropy > 0) relEntropy else 0
  }

  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine
    *
    * @param nodeArray
    * @param maxLength
    * @param stringProbs
    */
  def calcStringProbs(nodeArray:Array[CausalState],
                      maxLength:Int,
                      S:Array[CausalState],
                      alphabet:ParseAlphabet,
                      stringProbs:DenseVector[Double]
                      ) {
    for ((history, i) <- nodeArray.view.zipWithIndex) {
      stringProbs(i) = calcStringProb(history.observed, S, alphabet)
    }
  }

  /**
    * calculates the probability of a string in the data based on the inferred machine
    *
    * @param string
    * @param S
    * @param alphabet
    * @return
    */
  def calcStringProb(string:String, S:Array[CausalState], alphabet:ParseAlphabet) = {
    var totalPerString:Double = 0
//    var currentState:CausalState = _
//    val index:Int = _
    var frequency: Double = 0
    var transition:Double = 0

    for (s <- S) {
      val startState = s
      var totalPerState:Double = 1
      frequency = s.totalCounts
      var currentState = startState
      var isNullTrans = false
      for (symbol:Char <- string if !isNullTrans) {
        val index = alphabet.map(symbol)
        //get transition probability from current state
        totalPerState = totalPerState * currentState.distribution(index)

        //make transition
        transition = s.distribution(index)
        if (transition == 0) {
          totalPerState = 0
          isNullTrans = true
        } else {
          currentState = S(index)
        }
      }
      totalPerString += frequency * totalPerState
    }
    totalPerString
  }

  /*
  void Machine::CalcRelEntRate(ParseTree& parsetree, HashTable2* hashtable, bool isMulti) {
  G_Array g_array;
  int dataSize = parsetree.getDataSize();
  int alphaSize = parsetree.getAlphaSize();
  int maxLength = parsetree.getMaxLength();
  int adjustedDataSize = parsetree.getAdjustedDataSize();
  // We can't begin a substring of length maxLength at the last (maxLength-1)
  // positions in the data string, so we adjust the data size
  parsetree.FindStrings(maxLength - 1, &g_array);
  int size = g_array.getSize();
  double* stringProbs = new double[size];
  ArrayElem** list = g_array.getList();
  double totalRelEntRate = 0;
  double relEntRateHist = 0;
  double childStringProb = 0;
  char* alpha = parsetree.getAlpha();

  //determine the inferred distributions of max - 1 length strings
  CalcStringProbs(&g_array, maxLength - 1, hashtable, stringProbs);

  //for each string
  for(int i = 0; i< size; i++) {
    relEntRateHist = CalcRelEntRateHist(stringProbs, list, hashtable, i, alpha, alphaSize, adjustedDataSize);

    totalRelEntRate +=relEntRateHist;
  }

  //convert to binary
  m_relEntRate = totalRelEntRate/log(2);
  delete[] stringProbs;
}
   */
/*
double Machine::CalcRelEntRateHist(double* stringProbs,
                                   ArrayElem** list,
                                   HashTable2* hashtable,
                                   int index,
                                   char* alpha,
                                   int alphaSize,
                                   int adjustedDataSize
                                   ) {
  int* counts= list[index]->getCounts();
  double histFrequency = 0;
  double relEntRateAlpha = 0;
  double relEntRateHist = 0;
  double accumulatedInferredRatio = 0;
  double dataDist;
  double stringProb;
  char* history;
  char alphaElem;

  for(int j = 0; j< alphaSize;j++ ) {
    histFrequency +=((double) counts[j]);
  }
  //for each alpha value/symbol
  for(int k  = 0; k < alphaSize; k++) {
    //get distribution for data
    dataDist = ((double)counts[k])/histFrequency;
    stringProb = stringProbs[index];
    history = list[index]->getString();
    alphaElem = alpha[k];
    relEntRateAlpha = CalcRelEntRateAlpha(stringProb, history, accumulatedInferredRatio, dataDist, alphaElem, hashtable);
    relEntRateHist +=  relEntRateAlpha;
  }

  //correct for underflow error
  if (relEntRateHist < 0) {
    relEntRateHist = 0;
  }

  histFrequency = (double)(histFrequency/((double)adjustedDataSize));
  return histFrequency*relEntRateHist;
}
 */
  /*
  double Machine::CalcRelEntRateAlpha(double stringProb,
                                    char* history,
                                    double& accumulatedInferredRatio,
                                    double dataDist, char alphaElem,
                                    HashTable2* hashtable
                                    ) {
  double logRatio = 0;
  double relEntRateAlpha = 0;
  double childStringProb = 0;
  char* symbol = new char[2];
  symbol[1] = '\0';
  double inferredRatio = 0;
  char* tempChildString;

  //if child string appears in data
  if(dataDist > 0) {
    //get distribution for machine
    symbol[0] = alphaElem;
    tempChildString = new char[strlen(history) + 2];
    strcpy(tempChildString, history);
    strcat(tempChildString, symbol);

    //determine the inferred distribution for the value given the history
    childStringProb = CalcStringProb(tempChildString, hashtable);
    if(stringProb > 0) {
      inferredRatio = childStringProb/(stringProb);
      accumulatedInferredRatio += inferredRatio;

    //neglect this ratio but continue with program
    } else {
      cerr << "\nWarning: Inferred machine says actual history ("
           << history << ") is"
           << " impossible in Machine::CalcRelEntRateAlpha.\n\n"
           << "Note: It is likely that this sequence only occurs once, in the "
           << "beginning of the data, and has been treated as transitory by "
           << "the code and mistakenly deleted from the machine. See 'ReadMe' for details.\n"
           << endl;
    }

    //take the log ratio between the
    //conditional distributions of the inferred and data
    logRatio = log(dataDist/inferredRatio);

    //multiply by the conditional distribition from the data
    relEntRateAlpha = dataDist*logRatio;

    delete[] tempChildString;
  } else {
    relEntRateAlpha = 0;
  }

  delete[] symbol;
  return relEntRateAlpha;
}
   */
  /*
  void Machine::CalcVariation(ParseTree& parsetree, HashTable2* hashtable, bool isMulti) {
  G_Array g_array;
  int dataSize = parsetree.getDataSize();
  int alphaSize = parsetree.getAlphaSize();
  int maxLength = parsetree.getMaxLength();
  int adjustedDataSize = parsetree.getAdjustedDataSize();
  // We can't begin a substring of length maxLength at the last (maxLength-1)
  // positions in the data string
  int* counts;
  double histFrequency;
  parsetree.FindStrings(maxLength, &g_array);
  int size = g_array.getSize();
  double* stringProbs = new double[size];
  ArrayElem** list = g_array.getList();
  double total= 0;
  double dataDist;
  double diffHist = 0;

  //determine the inferred distributions of max - 1 length strings
  CalcStringProbs(&g_array, maxLength, hashtable, stringProbs);

  //for each string
  for(int i = 0; i< size; i++) {
    counts = list[i]->getCounts();
    histFrequency = 0;
    diffHist = 0;

    //for each alpha value/symbol
    for(int k  = 0; k < alphaSize; k++) {
      //get distribution for data
      dataDist = ((double)counts[k])/((double)adjustedDataSize);
      histFrequency += dataDist;
    }

    //take the differnece between the
    //inferred frequency and data frequency
    diffHist = fabs(histFrequency - stringProbs[i]);
    total += diffHist;
  }
  m_variation = total;
  delete[] stringProbs;
}
   */


  /*
  void Machine::CalcCmu() {
  State* state = NULL;
  int size = m_allstates->getArraySize();
  double cMu = 0;
  double prob = 0;

  for(int i = 0; i < size; i++) {
    state = m_allstates->getState(i);
    prob = state->getFrequency();
    if (prob) {
      cMu += prob*(log(prob)/log(2));
    }
  }
  m_cMu = -cMu;
}
   */

  def calcEntRate(S:Array[EquivalenceClass]):Double = {
    val size = S.length
    var entRate:Double = 0
    var prob:Double = 0
    var freq:Double = 0

    for (s <- S; (i, prob) <- s.distribution.activeIterator ) {
      s.frequency
      if (prob <= 0) {
        entRate += freq * (prob * (math.log(prob) / math.log(2)))
      }
    }
    return -entRate
  }
}

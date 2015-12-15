package com.typeclassified.hmm.cssr.measure

object RelativeEntropyRate {
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
}

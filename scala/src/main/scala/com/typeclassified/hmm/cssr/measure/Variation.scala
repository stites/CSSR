package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.parse.{Alphabet, Tree}

object Variation {
  def calculateVariation(parseTree:Tree, hashtable:Alphabet, isMulti:Boolean) = {

  }
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

}

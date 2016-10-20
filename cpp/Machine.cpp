/////////////////////////////////////////////////////////////////////////////
//Title:	Machine.cpp
//Author:	Kristina Klinkner
//Date:		July 23, 2003
//Description:  Class which contains methods for computing state machine and
//              associated values for array of states from class AllStates
//              for use with CSSR
/////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
//
//    Copyright (C) 2002 Kristina Klinkner
//    This file is part of CSSR
//
//    CSSR is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    CSSR is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with CSSR; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//////////////////////////////////////////////////////////////////////////////

#include "Machine.h"

/////////////////////////////////////////////////////////////////////////////

Machine::Machine(AllStates *allstates) {
  m_allstates = allstates;
  m_relEnt = 0;
  m_variation = 0;
  m_cMu = 0;
  m_entRate = 0;
  m_relEntRate = 0;

}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcStringProbs
// Purpose: calculates the probability of all the max length strings in the
//          data based on the inferred machine
// In Params: the array of states (machine), array of max length strings
//            and their conditional frequencies, a hashtable of alpha values
//            and their indices
// Out Params: a pointer to an array of the string probabilities
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the string probabilities have been calculated and stored
//            in an array
//////////////////////////////////////////////////////////////////////////
void Machine::CalcStringProbs(G_Array *g_array, int maxLength,
                              HashTable2 *hashtable, double stringProbs[]) {
  int stringArraySize = g_array->getSize();
  char *string = NULL;

  for (int k = 0; k < stringArraySize; k++) {
    string = (g_array->getList())[k]->getString();
    stringProbs[k] = CalcStringProb(string, hashtable);
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcStringProb
// Purpose: calculates the probability of a string in the
//          data based on the inferred machine
// In Params: the array of states (machine), a string and 
//            a hashtable of alpha values and their indices
// Out Params: the string probability
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the string probability been calculated and returned to the
//            calling function
//////////////////////////////////////////////////////////////////////////
double Machine::CalcStringProb(char *string, HashTable2 *hashtable) {
  double totalPerString = 0;
  double totalPerState;
  double total = 0;
  State *currentState;
  State *startState;
  int stateArraySize = m_allstates->getArraySize();
  int index;
  double frequency;
  char *symbol = new char[2];
  symbol[1] = '\0';
  bool isNullTrans = false;
  int transition;
  int length = strlen(string);

  for (int i = 0; i < stateArraySize; i++) {
    totalPerState = 1;
    startState = m_allstates->getState(i);
    frequency = startState->getFrequency();
    currentState = startState;
    isNullTrans = false;
    for (int j = 0; j < length && !isNullTrans; j++) {
      //get index of next alpha symbol
      symbol[0] = string[j];
      index = hashtable->WhichIndex(symbol);
      //get transition probability from current state
      totalPerState = totalPerState * (currentState->
          getCurrentDist())[index];

      //make transition
      transition = currentState->getTransitions(index);
      if (transition == NULL_STATE) {
        totalPerState = 0.0;
        isNullTrans = true;
      }
      else {
        currentState = m_allstates->getState(transition);
      }
    }
    totalPerString += frequency * totalPerState;
  }
  delete[] symbol;
  return totalPerString;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcRelEnt
// Purpose: calculates the relative entropy based on the inferred machine
// In Params: parstree of strings, hashtable of alpha and index values,
//            boolean denoting multi-string input
// Out Params: none
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the relative entropy has been calculated and stored in the 
//            machine class as a member variable
//////////////////////////////////////////////////////////////////////////
void Machine::CalcRelEnt(ParseTree &parsetree, HashTable2 *hashtable, bool isMulti) {
  G_Array g_array;
  int dataSize = parsetree.getDataSize();
  int alphaSize = parsetree.getAlphaSize();
  int maxLength = parsetree.getMaxLength();
  int adjustedDataSize = parsetree.getAdjustedDataSize();
  // We can't begin a substring of length maxLength at the last (maxLength-1)
  // positions in the data string
  double relEntropy = 0;
  double dataProb;
  double logRatio;
  int *counts;
  int occurrence;
  parsetree.FindStrings(maxLength, &g_array);
  int size = g_array.getSize();
  double *stringProbs = new double[size];
  ArrayElem **list = g_array.getList();

  CalcStringProbs(&g_array, maxLength, hashtable, stringProbs);

  for (int i = 0; i < size; i++) {
    occurrence = 0;
    counts = ((list[i])->getCounts());
    for (int k = 0; k < alphaSize; k++) {
      occurrence += counts[k];
    }

    dataProb = ((double) occurrence) / ((double) adjustedDataSize);

    if (dataProb) {
      logRatio = log(dataProb) - log(stringProbs[i]);
      logRatio *= dataProb;
      relEntropy += logRatio;
    }
  }

  relEntropy = relEntropy / log(2);
  m_relEnt = 0;
  if (relEntropy > 0) {
    m_relEnt = relEntropy;
  }

  delete[] stringProbs;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcRelEntRate
// Purpose: calculates the relative entropy rate based on the inferred machine
// In Params: parstree of strings, hashtable of alpha and index values, 
//            and boolean denoting multi-string input
// Out Params: none
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the relative entropy rate has been calculated and stored in the 
//            machine class as a member variable
//////////////////////////////////////////////////////////////////////////
void Machine::CalcRelEntRate(ParseTree &parsetree, HashTable2 *hashtable, bool isMulti) {
  G_Array g_array;
  int dataSize = parsetree.getDataSize();
  int alphaSize = parsetree.getAlphaSize();
  int maxLength = parsetree.getMaxLength();
  int adjustedDataSize = parsetree.getAdjustedDataSize();
  // We can't begin a substring of length maxLength at the last (maxLength-1)
  // positions in the data string, so we adjust the data size
  parsetree.FindStrings(maxLength - 1, &g_array);
  int size = g_array.getSize();
  double *stringProbs = new double[size];
  ArrayElem **list = g_array.getList();
  double totalRelEntRate = 0;
  double relEntRateHist = 0;
  double childStringProb = 0;
  char *alpha = parsetree.getAlpha();

  //determine the inferred distributions of max - 1 length strings
  CalcStringProbs(&g_array, maxLength - 1, hashtable, stringProbs);

  //for each string
  for (int i = 0; i < size; i++) {
    relEntRateHist = CalcRelEntRateHist(stringProbs, list, hashtable, i, alpha, alphaSize, adjustedDataSize);

    totalRelEntRate += relEntRateHist;
  }

  //convert to binary
  m_relEntRate = totalRelEntRate / log(2);
  delete[] stringProbs;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcRelEntRateHist
// Purpose: calculates the relative entropy rate based on the inferred machine 
//          for a given history
// In Params: an array of history probabilities, a list of histories, hashtable
//            of alpha and index values, index of current history, alphabet,
//            size of alphabet, the adjusted size of the data
// Out Params: the relative entropy rate of the history
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the relative entropy rate of the history has been calculated
//            and returned to the calling function
//////////////////////////////////////////////////////////////////////////
double Machine::CalcRelEntRateHist(double *stringProbs,
                                   ArrayElem **list,
                                   HashTable2 *hashtable,
                                   int index,
                                   char *alpha,
                                   int alphaSize,
                                   int adjustedDataSize
) {
  int *counts = list[index]->getCounts();
  double histFrequency = 0;
  double relEntRateAlpha = 0;
  double relEntRateHist = 0;
  double accumulatedInferredRatio = 0;
  double dataDist;
  double stringProb;
  char *history;
  char alphaElem;

  for (int j = 0; j < alphaSize; j++) {
    histFrequency += ((double) counts[j]);
  }
  //for each alpha value/symbol 
  for (int k = 0; k < alphaSize; k++) {
    //get distribution for data
    dataDist = ((double) counts[k]) / histFrequency;
    stringProb = stringProbs[index];
    history = list[index]->getString();
    alphaElem = alpha[k];
    relEntRateAlpha = CalcRelEntRateAlpha(stringProb, history, accumulatedInferredRatio, dataDist, alphaElem,
                                          hashtable);
    relEntRateHist += relEntRateAlpha;
  }

  //correct for underflow error
  if (relEntRateHist < 0) {
    relEntRateHist = 0;
  }

  histFrequency = (double) (histFrequency / ((double) adjustedDataSize));
  return histFrequency * relEntRateHist;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcRelEntRateAlpha
// Purpose: calculates the relative entropy rate based on the inferred machine 
//          for one alphabet symbol given a specific history
// In Params: the history probability, the history, the frequency of occurence
//            of the history with that particular alpha symbol, alphabet
//            symbol,   hashtable of alpha and index values,
// Out Params: the relative entropy rate of the history
// In/Out Params: the accumulated inferred ratio over alpha symbols
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the relative entropy rate of the symbol given a history has been
//            calculated and returned to the calling function
//////////////////////////////////////////////////////////////////////////
double Machine::CalcRelEntRateAlpha(double stringProb,
                                    char *history,
                                    double &accumulatedInferredRatio,
                                    double dataDist, char alphaElem,
                                    HashTable2 *hashtable
) {
  double logRatio = 0;
  double relEntRateAlpha = 0;
  double childStringProb = 0;
  char *symbol = new char[2];
  symbol[1] = '\0';
  double inferredRatio = 0;
  char *tempChildString;

  //if child string appears in data
  if (dataDist > 0) {
    //get distribution for machine
    symbol[0] = alphaElem;
    tempChildString = new char[strlen(history) + 2];
    strcpy(tempChildString, history);
    strcat(tempChildString, symbol);

    //determine the inferred distribution for the value given the history
    childStringProb = CalcStringProb(tempChildString, hashtable);
    if (stringProb > 0) {
      inferredRatio = childStringProb / (stringProb);
      accumulatedInferredRatio += inferredRatio;

      //neglect this ratio but continue with program
    }
    else {
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
    logRatio = log(dataDist / inferredRatio);

    //multiply by the conditional distribition from the data
    relEntRateAlpha = dataDist * logRatio;

    delete[] tempChildString;
  }
  else {
    relEntRateAlpha = 0;
  }

  delete[] symbol;
  return relEntRateAlpha;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcVariation
// Purpose: calculates the variation rate based on the inferred machine
// In Params: parstree of strings, hashtable of alpha and index values, 
//            and boolean denoting multi-string input
// Out Params: none
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the relative entropy rate has been calculated and stored in the 
//            machine class as a member variable
//////////////////////////////////////////////////////////////////////////
void Machine::CalcVariation(ParseTree &parsetree, HashTable2 *hashtable, bool isMulti) {
  G_Array g_array;
  int dataSize = parsetree.getDataSize();
  int alphaSize = parsetree.getAlphaSize();
  int maxLength = parsetree.getMaxLength();
  int adjustedDataSize = parsetree.getAdjustedDataSize();
  // We can't begin a substring of length maxLength at the last (maxLength-1)
  // positions in the data string
  int *counts;
  double histFrequency;
  parsetree.FindStrings(maxLength, &g_array);
  int size = g_array.getSize();
  double *stringProbs = new double[size];
  ArrayElem **list = g_array.getList();
  double total = 0;
  double dataDist;
  double diffHist = 0;

  //determine the inferred distributions of max - 1 length strings
  CalcStringProbs(&g_array, maxLength, hashtable, stringProbs);

  //for each string
  for (int i = 0; i < size; i++) {
    counts = list[i]->getCounts();
    histFrequency = 0;
    diffHist = 0;

    //for each alpha value/symbol
    for (int k = 0; k < alphaSize; k++) {
      //get distribution for data
      dataDist = ((double) counts[k]) / ((double) adjustedDataSize);
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


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcCmu
// Purpose: calculates the statistical complexity based on the inferred machine
// In Params: the array of states (machine)
// Out Params: none
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the statistical complexity has been calculated and stored in the 
//            machine class as a member variable
//////////////////////////////////////////////////////////////////////////
void Machine::CalcCmu() {
  State *state = NULL;
  int size = m_allstates->getArraySize();
  double cMu = 0;
  double prob = 0;

  for (int i = 0; i < size; i++) {
    state = m_allstates->getState(i);
    prob = state->getFrequency();
    if (prob) {
      cMu += prob * (log(prob) / log(2));
    }
  }
  m_cMu = -cMu;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::CalcEntRate
// Purpose: calculates the entropy rate based on the inferred machine
// In Params: the array of states (machine)
// Out Params: none
// In/Out Params: none
// Pre- Cond: array of states (the machine) has been inferred
// Post-Cond: the entropy rate has been calculated and stored in the 
//            machine class as a member variable
//////////////////////////////////////////////////////////////////////////
void Machine::CalcEntRate() {
  State *state = NULL;
  int size = m_allstates->getArraySize();
  double entRate = 0;
  double prob = 0;
  int distSize = m_allstates->getDistSize();
  double freq = 0;

  for (int i = 0; i < size; i++) {
    state = m_allstates->getState(i);
    freq = state->getFrequency();
    for (int k = 0; k < distSize; k++) {
      prob = ((state->getCurrentDist())[k]);
      if (prob) {
        entRate += freq * (prob * (log(prob) / log(2)));
      }
    }
  }
  m_entRate = -entRate;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::PrintOut
// Purpose: prints out all the information values calculated by machine
//          to a file
// In Params: the name of the input file used for program
// Out Params: none
// In/Out Params: none
// Pre- Cond: relative entropy, statistical complexity, entropy rate have
//            been calculated
// Post-Cond: output file exists with information listed inside
//////////////////////////////////////////////////////////////////////////
void Machine::PrintOut(char input[],
                       const char *alpha_file,
                       const char *data_file,
                       const int &max_length,
                       const double &sigLevel,
                       const bool &isMulti,
                       const bool &isChi,
                       int alphaSize
) {
  char *output = new char[strlen(input) + 6];

  strcpy(output, input);
  strcat(output, "_info");

  //create file streams
  ofstream outData(output, ios::out);

  //open output file, if unsuccessful set boolean return value
  if (!outData) {
    cerr << " the information output file cannot be opened " << endl;
    exit(1);
    //otherwise output data
  }
  else {
    outData << "Alphabet File: " << alpha_file << endl;
    outData << "Data File: " << data_file << endl;
    outData << "History Length: " << max_length << endl;
    outData << "Significance Level: " << sigLevel << endl;
    outData << "Multiline Mode: " << (isMulti ? "true" : "false") << endl;
    outData << "Chi-squared test used: " << (isChi ? "true" : "false") << endl;
    outData << "Alphabet Size: " << alphaSize << endl;
    outData << "Relative Entropy: " << m_relEnt << endl;
    outData << "Relative Entropy Rate: " << m_relEntRate << endl;
    outData << "Statistical Complexity: " << m_cMu << endl;
    outData << "Entropy Rate: " << m_entRate << endl;
    outData << "Variation: " << m_variation << endl;
    outData << "Number of Inferred States: " << m_allstates->getArraySize()
    << endl;

    if (m_allstates->getReSynch()) {
      outData << "This data needed to be synchronized to a set"
      << " of states more than once.  It is recommended that"
      << " you try a longer history length, as this set of"
      << " states cannot possibly be the causal states." << endl;

      //Print to screen also
      cout << endl
      << "The data in " << input << endl
      << "needed to be resynchronized; try to"
      << " increase max length." << endl << endl;
    }
  }
  outData.close();
  delete[] output;
}


///////////////////////////////////////////////////////////////////////////
// Function: Machine::PrintDot
// Purpose: prints out all the machine to a .dot file
// In Params: the name of the input file used for program, the alphabet
// Out Params: none
// In/Out Params: none
// Pre- Cond: all values in array of states have been set
// Post-Cond: output .dot file exists with machine
//////////////////////////////////////////////////////////////////////////
void Machine::PrintDot(char input[], char alpha[]) {
  char *output = new char[strlen(input) + 9];

  strcpy(output, input);
  strcat(output, "_inf.dot");

  //create file streams
  ofstream outData(output, ios::out);

  //open output file, if unsuccessful exit
  if (!outData) {
    cerr << " the .dot output file cannot be opened " << endl;
    exit(1);

    //otherwise output data
  }
  else {
    int size = m_allstates->getArraySize();
    int distSize = m_allstates->getDistSize();
    State *state;
    int nextState;
    double *dist;

    outData << "digraph \"" << input << "\" {" << endl;
    outData << "size = \"6,8.5\";" << endl;
    outData << "ratio = \"fill\";" << endl;
    outData << "node [shape = circle];" << endl;
    outData << "node [fontsize = 24];" << endl;
    outData << "edge [fontsize = 24];" << endl;

    for (int i = 0; i < size; i++) {
      state = m_allstates->getState(i);
      for (int k = 0; k < distSize; k++) {
        dist = state->getCurrentDist();
        nextState = state->getTransitions(k);
        if (nextState != NULL_STATE) {
          outData << i << " -> "
          << nextState;
          outData << " [label = \"" << alpha[k]
          << ": " << setiosflags(ios::left) << setw(7) << setprecision(4) << dist[k] << "  \"];" << endl;
        }
      }
    }
    outData << "}";
  }
  outData.close();
  delete[] output;
}

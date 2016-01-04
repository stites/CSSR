/////////////////////////////////////////////////////////////////////////////
//Title:	AllStates.cpp
//Author:	Kristina Klinkner
//Date:		July 23, 2003
//Description:  Class which contains and controls array of states
//              for program 'CSSR'
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




#include "AllStates.h"


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::PrintOut
// Purpose: prints out all states in array to a file
// In Params: the name of the input file used for program, the alphabet
// Out Params: none
// In/Out Params: none
// Pre- Cond: all states have been created up to maximum length of strings
// Post-Cond: output file exists with states listed inside
//////////////////////////////////////////////////////////////////////////
void AllStates::PrintOut(char input[], char alpha[]) {
  bool success = true;
  char *output = new char[strlen(input) + 8 + END_STRING];

  strcpy(output, input);
  strcat(output, "_results");

  //create file streams
  ofstream outData(output, ios::out);

  //open output file, if unsuccessful set boolean return value
  if (!outData) {
    cerr << " the results output file cannot be opened " << endl;
    success = false;
  }
  else {
    //otherwise output data
    for (int i = 0; i < m_arraySize; i++) {
      outData << "State number: " << m_StateArray[i]->getNumber() << endl;
      m_StateArray[i]->PrintStringList(&outData, alpha);
    }
  }

  outData.close();
  delete[] output;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::Grow
// Purpose: enlarges array of states by 'INCREMENT'
// In Params: size of new array
// Out Params: new array
// In/Out Params: old array
// Pre- Cond: Array of states is full, m_arraySize = m_maxArraySize -1
// Post-Cond: Array is now able to insert new states
//////////////////////////////////////////////////////////////////////////
void AllStates::Grow(int newsize) {
  //check if index is valid
  State **newBuffer = NULL;
  int i;

  newBuffer = new State *[newsize];

  if (newBuffer == NULL) {
    cerr << "Out of memory." << endl;
    exit(1);
  }

  //set each pointer to NULL
  for (i = 0; i < newsize; i++) {
    newBuffer[i] = NULL;
  }

  //copy buffer contents into newBuffer
  for (i = 0; i <= m_arraySize; i++) {
    newBuffer[i] = m_StateArray[i];
  }

  delete[] m_StateArray;
  m_StateArray = NULL;

  //point the array buffer at the newly created buffer
  m_StateArray = newBuffer;
  m_maxArraySize = newsize;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::Insert
// Purpose: adds a new array element to the specified state
// In Params: string for new state, state in which to insert the string
// Out Params: new state
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: Array of states has been initialized, new distribution has
//            been checked for uniqueness
// Post-Cond: string has been added to new state, new state added to array
//            array size increased by one
///////////////////////////////////////////////////////////////////////////
void AllStates::Insert(ArrayElem *elem, State *state) {
  state->Insert(elem, m_table);
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::Insert
// Purpose: adds a new state to the array of states, or attaches a string
//          to existing state
// In Params: array element for new state, index of insertion for elem
// Out Params: new state
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: Array of states has been initialized, new distribution has
//            been checked for uniqueness
// Post-Cond: string has been added to new state, new state added to array
//            array size increased by one
///////////////////////////////////////////////////////////////////////////
void AllStates::Insert(ArrayElem *elem, int index) {
  //to add a new state
  if (index == m_arraySize) {
    if (Is_Full()) {
      Grow(m_maxArraySize + INCREMENT);
    }

    State *temp = new State(m_distSize, m_arraySize);
    m_StateArray[m_arraySize] = temp;
    m_StateArray[m_arraySize]->Insert(elem, m_table);
    m_arraySize++;
  }
  else {
    //otherwise add a string to an existing state
    m_StateArray[index]->Insert(elem, m_table);
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::Insert
// Purpose: adds a new state to the array of states, or attaches a string
//          to existing state
// In Params: string element for new state, index of insertion for elem
// Out Params: new state
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: Array of states has been initialized, new distribution has
//            been checked for uniqueness
// Post-Cond: string has been added to new state, new state added to array
//            array size increased by one
///////////////////////////////////////////////////////////////////////////
void AllStates::Insert(StringElem *elem, int index) {
  //to add a new state
  if (index == m_arraySize) {
    if (Is_Full()) {
      Grow(m_maxArraySize + INCREMENT);
    }
    State *temp = new State(m_distSize, m_arraySize);
    m_StateArray[m_arraySize] = temp;
    m_StateArray[m_arraySize]->Insert(elem, m_table);
    m_arraySize++;
  }
  else {
    //otherwise add a string to an existing state
    m_StateArray[index]->Insert(elem, m_table);
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::CalcNewDist
// Purpose: calculates the frequency of occurrence of each element given
//          new string - P(element|new string)and adds states as need be
// In Params: Parse tree of all strings, current length to examine
// Out Params: any new states created
// In/Out Params: old states which have strings added to them
// Pre- Cond: Parse tree is set, initial states  have been created
// Post-Cond: State has array of probabilities of occurence for each
//            element in the alphabet given new string
//////////////////////////////////////////////////////////////////////////
void AllStates::CalcNewDist(int length, ParseTree &parsetree) {
  G_Array g_array;
  double *newDist = new double[m_distSize];
  int stringCount;
  bool match;
  State *removalState;
  char *removalString;
  ArrayElem **list;
  int listSize;

  //get list containing all strings of proper length
  parsetree.FindStrings(length, &g_array);
  list = g_array.getList();
  listSize = g_array.getSize();

  //for all of the strings
  cout << "AllStates, List Size: " << std::to_string(listSize) << endl << endl;
  for (int i = 0; i < listSize; i++) {
    match = false;
    stringCount = 0;

    //calculate new distribution
    for (int k = 0; k < m_distSize; k++) {
      newDist[k] = (double) ((list[i]->getCounts())[k]);
      stringCount += (list[i]->getCounts())[k];
    }

    //divide by total string occurences
    for (int j = 0; j < m_distSize; j++) {
      newDist[j] = newDist[j] / (double) stringCount;
    }

    //check whether new distribution matches that of parent state
    match = CompareToParent(removalString, removalState, list[i], newDist, length, match, stringCount);

    //check whether new distribution matches that of another existing state
    match = RePartition(match, removalString, removalState, list[i], newDist, stringCount);

    //if there were no matches make new state
    if (!match) {
      //insert new string
      Insert(list[i], m_arraySize);

      //calculate probability distribution
      m_StateArray[m_arraySize - 1]->CalcCurrentDist();

      //remove ancestor-strings when progeny
      //creates new state
      RemoveAncestors(removalString, removalState);
    }
  }
  delete[] newDist;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::RemoveAncestors
// Purpose: removes the histories which have spawned new histories with
//          distinct distributions
// In Params: none
// Out Params: none
// In/Out Params: the history which has spawned a new distribution and
//                the parent state of that history
// Pre- Cond: Parse tree is set, initial states  have been created,
//            distribution for new history and for parent state are known
// Post-Cond: ancestors of string are removed from state; if state is left
//            empty it is then deleted.
//////////////////////////////////////////////////////////////////////////
void AllStates::RemoveAncestors(char *&removalString, State *&removalState) {
  int removalLength;

  if (removalState != NULL) {
    if (removalString) {
      removalLength = strlen(removalString);
    }
    else {
      removalLength = 0;
    }

    while (removalState != NULL && removalLength > 0) {
      //remove ancestor-string
      m_table->RemoveString(removalString);
      //if state is empty remove it
      if (!removalState->getStringList()) {
        RemoveState(removalState->getNumber());
      }
      else {
        //re-caculate ancestor-state's distribution
        removalState->CalcCurrentDist();
      }
      removalString++;
      removalState = m_table->WhichState(removalString);
      removalLength--;
    }
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::CompareToParent
// Purpose: checks the distribution of the new history against that of its
//          parent state
// In Params: array element containing the appropriate history, distribution of
//            that history, count of that history, length of that history
// Out Params: boolean signifying whether the history matched its parent
//             state's
// In/Out Params: the history which has spawned a new distribution and
//                the parent state of that history
// Pre- Cond: Parse tree is set, initial states  have been created,
//            distribution for new history and for parent state are known
// Post-Cond: history has been added to parent state if it matched, boolean
//            has been set
//////////////////////////////////////////////////////////////////////////
bool AllStates::CompareToParent(char *&removalString, State *&removalState,
                                ArrayElem *element, double *newDist,
                                int length, bool match, int stringCount) {
  double sigLevel;

  //Compare new distributions to parent State first
  removalString = element->getString();

  if (length > 1) {
    (removalString)++;
  }
  else {
    (removalString) = "NULL";
  }

  (removalState) = m_table->WhichState(removalString);
  if (removalState != NULL) {
    sigLevel = Compare(removalState, newDist, stringCount);
    //if not significantly different from parent state
    if (sigLevel >= m_sigLevel) {
      match = true;
      //add new string to state
      Insert(element, removalState);
      //re-calculate probability distribution
      (removalState)->CalcCurrentDist();
    }
  }
  return match;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::RePartition
// Purpose: checks the distribution of the new history against that of all
//          non-parent states
// In Params: array element containing the appropriate history, distribution of
//            that history, count of that history
// Out Params: boolean signifying whether the history matched its parent
//             state's
// In/Out Params: the history which has spawned a new distribution and
//                the parent state of that history
// Pre- Cond: Parse tree is set, initial states  have been created,
//            distribution for new history and for non-parent states are known
// Post-Cond: history has been added to non-parent state if it matched, boolean
//            has been set
//////////////////////////////////////////////////////////////////////////
bool AllStates::RePartition(bool match, char *removalString,
                            State *removalState, ArrayElem *element,
                            double *newDist, int stringCount) {
  double sigLevel;

  //compare new distribution to all other states
  for (int q = 0; q < m_arraySize && match == false; q++) {
    //test distribution against that of current state
    sigLevel = Compare(q, newDist, stringCount);
    //if not significantly different from state,
    //add to that state
    if (sigLevel >= m_sigLevel) {
      match = true;
      Insert(element, q);
      //re-calculate probability distribution
      m_StateArray[q]->CalcCurrentDist();
    }
  }
  return match;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::Determinize
// Purpose: splits states which have strings with differing futures
//          for the same symbol
// In Params:parse tree which contains alphabet of symbols and max length
// Out Params: any new states created
// In/Out Params: old states which have been changed
// Pre- Cond: states have been created based on pasts of maximum length
// Post-Cond: States all have consistent transitions for a given symbol
///////////////////////////////////////////////////////////////////////////
void AllStates::Determinize(ParseTree &parsetree) {
  StringElem **stringArray = NULL;
  int **stateArray = new int *[m_distSize];
  int arraySize;
  bool isDeterministic; //variable to check whether states are not created
  int maxLength = parsetree.getMaxLength();
  bool firstPass = true;
  bool isStatesRemoved;

  for (int q = 0; q < m_distSize; q++) {
    stateArray[q] = NULL;
  }

  do {
    //no states created yet
    isDeterministic = true;

    //no states deleted due to only long strings yet
    isStatesRemoved = false;

    //for each state
    for (int i = 0; i < m_arraySize; i++) {
      //make 2-D array of state values and 1-D array of
      //string values
      arraySize = m_StateArray[i]->getListSize();
      if (arraySize > 0) {
        //allocate and initialize arrays
        AllocateArray(stateArray, arraySize);
        stringArray = new StringElem *[arraySize];
        for (int k = 0; k < arraySize; k++) {
          stringArray[k] = NULL;
        }

        //fill arrays with transition information
        CreateChildStateArray(parsetree, arraySize, stateArray, stringArray, i);

        //check for non-determinism
        //and create new states to make deterministic
        isDeterministic = MakeNewDetStates(i, stringArray,
                                           stateArray, arraySize,
                                           isDeterministic, parsetree);

        //deallocate arrays
        for (int j = 0; j < arraySize; j++) {
          delete stringArray[j];
          stringArray[j] = NULL;
        }
        delete[] stringArray;
        stringArray = NULL;
        DeAllocateArray(stateArray);
      }
    }
  } while (isDeterministic == false || isStatesRemoved == true);

  delete[] stateArray;
  stateArray = NULL;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::MakeNewDetStates
// Purpose: creates new states by determinism
// In Params: index of state to determinize, 2-D array of histories in the
//            state and the children of those histories, 2-D index of states
//            the child histories lead to, the size of the state, the parsetree
// Out Params: boolean signifying whether the state is already deterministic
// In/Out Params: none
// Pre- Cond: States have been set based on distribution and connected
//            components
// Post-Cond: State has been passed through at least once for determinization
//////////////////////////////////////////////////////////////////////////
bool AllStates::MakeNewDetStates(int stateIndex, StringElem **stringArray,
                                 int **stateArray, int stringMax,
                                 bool isDeterministic, ParseTree &parsetree) {
  bool isFirstPass = true;
  StringElem *temp = NULL;
  int childState = 0;
  int tempSize = 0;
  bool isIncreased = false;   //variable to check whether
  //more states have been created
  bool isNewTransitions = false;

  //leave those with first non-NULL state value
  //in present state, then
  //make all other necessary states
  //now that arrays are complete,
  //determine common transition states
  for (int alphaIndex = 0; alphaIndex < m_distSize; alphaIndex++) {
    isFirstPass = true;
    tempSize = m_arraySize;
    isIncreased = false;
    isNewTransitions = false;

    do {
      isNewTransitions = false;
      isFirstPass = true;

      for (int q = 0; (q < stringMax) && isNewTransitions == false; q++) {
        //for a state which has a transition,
        if (stateArray[alphaIndex][q] != NULL_STATE) {
          childState = stateArray[alphaIndex][q];
          stateArray[alphaIndex][q] = NULL_STATE;

          //after first time through, create new
          //states for those strings with differing transitions
          if (isFirstPass == false) {
            isIncreased = true;
            isDeterministic = false;
            temp = new StringElem(*stringArray[q]);
            m_table->RemoveString(stringArray[q]->m_string);
            Insert(temp, tempSize);
            delete temp;
          }

          FindSimilarTransitions(stringArray, stateArray, childState,
                                 tempSize, alphaIndex, q, stringMax,
                                 isFirstPass);

          //if another state has been created,
          // recalculate distributions
          if (isIncreased == true) {
            //fill arrays
            isNewTransitions = true;
            stringMax = m_StateArray[stateIndex]->getListSize();
            CreateChildStateArray(parsetree, stringMax, stateArray, stringArray, stateIndex);

            m_StateArray[tempSize]->CalcCurrentDist();
            m_StateArray[stateIndex]->CalcCurrentDist();
            tempSize++;
            isIncreased = false;
          }
          if (isFirstPass) {
            isFirstPass = false;
          }
        }
      }
    } while (isNewTransitions == true);
  }
  return isDeterministic;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::FindSimilarTransitions
// Purpose: finds histories which transition to the same state upon the same
//          symbol
// In Params: index of state which initial history has transitioned to, 2-D
//            array of histories in the state and the children of those
//            histories, 2-D index of states the child histories lead to, the
//            current index of the state, the size of the state, the index of
//            the symbol being examined, a boolean to show whether or not this
//            is the first check of these values
// Out Params: none
// In/Out Params: none
// Pre- Cond: States have been set based on distribution and connected
//            components
// Post-Cond: Given hisoty's transition has been matched to all similar
//            transitions
//////////////////////////////////////////////////////////////////////////
void AllStates::FindSimilarTransitions(StringElem **stringArray,
                                       int **stateArray, int childState,
                                       int tempSize, int alphaIndex,
                                       int stringIndex, int stringMax,
                                       bool isFirstPass) {
  StringElem *temp = NULL;

  for (int k = stringIndex + 1; k < stringMax; k++) {
    if (stateArray[alphaIndex][k] == childState) {
      stateArray[alphaIndex][k] = NULL_STATE;
      if (!isFirstPass) {
        temp = new StringElem(*stringArray[k]);
        m_table->RemoveString(stringArray[k]->m_string);
        Insert(temp, tempSize);
        delete temp;
      }
    }
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::DestroyLongHists
// Purpose: remove all histories of max length from the states
// In Params: the max length, the parsetree of histories
// Out Params: boolean signifying whether a state has been removed
// In/Out Params: none
// Pre- Cond: States are set
// Post-Cond: no histories of max length remain
///////////////////////////////////////////////////////////////////////////
bool AllStates::DestroyLongHists(int maxLength, ParseTree &parsetree) {
  StringElem *longHist;
  StringElem *deadHist;
  bool isStatesRemoved = false;

  //Check whether the shortest shistory in the state is
  //one less than the max, and set transitions here if so
  for (int i = 0; i < m_arraySize; i++) {
    longHist = m_StateArray[i]->getStringList();
    if (strlen(longHist->m_string) == maxLength - 1) {
      FindNSetTransitions(i, maxLength, parsetree.getAlpha());
    }
  }

  //for all states in array of states
  for (int j = 0; j < m_arraySize; j++) {
    longHist = m_StateArray[j]->getStringList();
    while ((longHist) && (strlen(longHist->m_string) < maxLength)) {
      longHist = longHist->m_nextPtr;
    }

    while (longHist) {
      deadHist = longHist;
      longHist = longHist->m_nextPtr;
      m_table->RemoveString(deadHist->m_string);
    }
  }
  //Remove any states which are now empty
  //due to max length history removal
  for (int k = 0; k < m_arraySize; k++) {
    if (!m_StateArray[k]->getStringList()) {
      RemoveState(k);
      isStatesRemoved = true;
    }
  }
  return isStatesRemoved;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::DestroyShortHists
// Purpose: remove all histories of less than max length - 1 from the states
// In Params: the max length, the parsetree of histories
// Out Params: boolean signifying whether a state has been removed
// In/Out Params: none
// Pre- Cond: States are set
// Post-Cond: no histories of less than max length - 1 remain
///////////////////////////////////////////////////////////////////////////
bool AllStates::DestroyShortHists(int maxLength, ParseTree &parsetree) {
  StringElem *shortHist;
  StringElem *deadHist;
  bool isStatesRemoved = false;

  //for all states in array of states
  for (int j = 0; j < m_arraySize; j++) {
    //check for 'NULL' string
    shortHist = m_StateArray[j]->getStringList();
    if (strcmp(shortHist->m_string, "NULL") == 0) {
      deadHist = shortHist;
      shortHist = shortHist->m_nextPtr;
      m_table->RemoveString(deadHist->m_string);
    }
    //check for histories which are too short
    while ((shortHist) && (strlen(shortHist->m_string) < maxLength - 1)) {
      deadHist = shortHist;
      shortHist = shortHist->m_nextPtr;
      m_table->RemoveString(deadHist->m_string);
    }
  }

  //Remove any states which are now empty
  //due to history removal
  for (int k = 0; k < m_arraySize; k++) {
    if (!m_StateArray[k]->getStringList()) {
      RemoveState(k);
      isStatesRemoved = true;
    }
  }
  return isStatesRemoved;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::StoreTransitions
// Purpose: stores symbol and corresponding transition for each state
// In Params:max length of strings, alphabet
// Out Params: none
// In/Out Params: none
// Pre- Cond: memory for state transitions have been allocated
// Post-Cond: state transition values have been set
///////////////////////////////////////////////////////////////////////////
void AllStates::StoreTransitions(int maxLength, char *alpha) {
  //for each state
  for (int i = 0; i < m_arraySize; i++) {
    FindNSetTransitions(i, maxLength, alpha);
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::FindNSetTransitions
// Purpose: sets the transitions of a state per given symbol
// In Params: the state to set, max history length in state, alphabet
// Out Params: none
// In/Out Params: none
// Pre- Cond: States have been set and determinized
// Post-Cond: State has a transition for each symbol
//////////////////////////////////////////////////////////////////////////
void AllStates::FindNSetTransitions(int state, int maxLength, char *alpha) {
  int shortestLength;
  StringElem *temp;
  int length;
  char *childString;
  char *symbol = new char[2];
  bool isNull;
  int childState;
  bool isTooLong;

  symbol[1] = '\0';
  temp = m_StateArray[state]->getStringList();

  shortestLength = strlen(temp->m_string) + 2;

  for (int k = 0; k < m_distSize; k++) {
    isTooLong = false;
    isNull = true;
    temp = m_StateArray[state]->getStringList();

    while (isNull == true && temp && isTooLong == false) {
      length = strlen(temp->m_string) + 2;
      childString = new char[length];
      strcpy(childString, temp->m_string);

      //if string exceeds max length allowed
      if ((length == maxLength + 2) && (shortestLength < (maxLength + 2))) {
        childState = NULL_STATE;
        isTooLong = true;
      }
      if (length > maxLength + 1) {
        childString++;
      }

      symbol[0] = alpha[k];

      //create child string
      strcat(childString, symbol);

      //determine state of child string
      childState = m_table->WhichStateNumber(childString);
      if (childState != NULL_STATE) {
        m_StateArray[state]->setTransitions
            (k, childState);

        isNull = false;
      }

      if (isNull == true && temp) {
        temp = temp->m_nextPtr;
      }

      if (length > maxLength + 1) {
        childString--;
      }

      delete[] childString;
    }

    //if no valid transitions were found, set to NULL_STATE
    if (isNull == true || isTooLong == true) {
      m_StateArray[state]->setTransitions(k, childState);
    }
  }
  delete[] symbol;
}


/////////////////////////////////////////////////////////////////////////
//Function: AllStates::DeAllocateArray
// Purpose: frees memory used for multidimensional array
// In Params: none
// Out Params: none
// In/Out Params: pointer to multidimensional array
// Pre- Cond: memory is allocated for array
// Post-Cond: memory is free
////////////////////////////////////////////////////////////////////////
void AllStates::DeAllocateArray(int **stateArray) {
  for (int i = 0; i < m_distSize; i++) {
    delete[] stateArray[i];
    stateArray[i] = NULL;
  }
}


/////////////////////////////////////////////////////////////////////////
//Function: AllStates::AllocateArray
// Purpose: allocates memory used for multidimensional array
// In Params: size of array to allocate
// Out Params: none
// In/Out Params: pointer to multidimensional array
// Pre- Cond: memory is allocated for main pointer (1-D array)
// Post-Cond: memory is allocated for each pointer (2-D array)
////////////////////////////////////////////////////////////////////////
void AllStates::AllocateArray(int **stateArray, int arraySize) {
  for (int i = 0; i < m_distSize; i++) {
    stateArray[i] = NULL;
    stateArray[i] = new int[arraySize];
    if (stateArray[i] == NULL) {
      cerr << "Out of memory\n";
      exit(1);
    }
    for (int j = 0; j < arraySize; j++) {
      stateArray[i][j] = NULL_STATE;
    }
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::CheckConnComponents
// Purpose: searches the machine for transient states and removes them
// In Params: the parse tree of history strings
// Out Params: none
// In/Out Params: none
// Pre- Cond: States have been set
// Post-Cond: All states are recurrent
//////////////////////////////////////////////////////////////////////////
void AllStates::CheckConnComponents(ParseTree &parsetree) {
  char *alpha = parsetree.getAlpha();
  int stateCounter = 0;
  int maxLength = parsetree.getMaxLength();
  bool done = false;
  int *stateArray = new int[m_arraySize];
  TransTable *transTable = NULL;

  //initialize array
  for (int i = 0; i < m_arraySize; i++) {
    stateArray[i] = NULL_STATE;
  }

  //keep doing until number of states stabilizes
  while (done == false) {
    transTable = new TransTable(m_arraySize);
    done = true;
    if (m_arraySize > 1) {
      //for all states
      for (int index = 0; index < m_arraySize; index++) {
        FillRecurrStateArray(alpha, maxLength, index, stateArray, stateCounter, transTable);
      }
      done = RemoveTransientStates(stateArray, done, stateCounter, transTable, alpha);
    }
    delete transTable;
  }
  delete[] stateArray;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::FillRecurrStateArray
// Purpose: fills an array with the states which are recurrent from
//          particular state which is examined
// In Params: alphabet, maximum length of history, index of specific
//            state to be examined
// Out Params: none
// In/Out Params: array of recurrent states, number of states which are
//                recurrent, table of transitons from max length strings
// Pre- Cond: Memory has been allocated for the array of recurrent states
// Post-Cond: The array of recurrent state indices has been set for the
//            specific state (array of 'childstates' of given state)
//////////////////////////////////////////////////////////////////////////
void AllStates::FillRecurrStateArray(char *alpha, int maxLength, int index,
                                     int *&stateArray, int &stateCounter,
                                     TransTable *&transTable) {
  int length;
  char *childString = NULL;
  int childState;
  char *symbol = new char[2];
  bool match = false;
  StringElem *temp;

  //use the list of strings for appropriate state
  temp = m_StateArray[index]->getStringList();

  while (temp != NULL) {
    //get string which is in list
    length = strlen(temp->m_string);

    //look at transitions of child strings
    for (int k = 0; k < m_distSize; k++) {
      childString = new char[length + 2 * SYMB_SIZE + END_STRING];
      strcpy(childString, temp->m_string);

      //wrap string if it exceeds max length
      if (length == maxLength) {
        childString++;
      }

      symbol[0] = alpha[k];
      symbol[1] = '\0';

      //create child string
      strcat(childString, symbol);

      //determine state of child string
      childState = m_table->WhichStateNumber(childString);

      //fill array with states to which there are transitions
      if (childState != index && ((length <= maxLength - 1) || temp == m_StateArray[index]->getStringList())) {
        //if state is already in array, ignore
        for (int q = 0; q < stateCounter; q++) {
          if (stateArray[q] == childState) {
            match = true;
          }
        }
        //if null state, ignore
        if (childState == NULL_STATE) {
          match = true;
        }

        //if not, add that state to the array
        if (match == false) {
          stateArray[stateCounter] = childState;
          stateCounter++;
        }
        match = false;
      }
      if (length == maxLength) {
        //record transitions from longest strings
        transTable->setTrans(childState, temp, index);
        childString--;
      }
      delete[] childString;
      childString = NULL;
    }
    temp = temp->m_nextPtr;
  }
  delete[] symbol;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::RemoveTransientStates
// Purpose: removes any transient states
// In Params: array of recurrent states, boolean to be set when none are
//            deleted, number of transient states
// Out Params: none
// In/Out Params: none
// Pre- Cond: Array of recurrent states has been set
// Post-Cond: Transient states have been deleted (though more may have been
//            created)
//////////////////////////////////////////////////////////////////////////
bool AllStates::RemoveTransientStates(int *stateArray, bool done,
                                      int stateCounter, TransTable *transTable,
                                      char *alpha) {
  StringElem *tempString;
  StringElem *tempString2;
  int removeAdjust;
  bool match = false;
  bool isUnique = false;
  TransNode *transTemp;
  int lowest = m_arraySize;

  removeAdjust = 0;

  //remove any states not found
  for (int z = 0; z < m_arraySize; z++) {
    match = false;
    for (int x = 0; x < stateCounter && match == false; x++) {
      if (stateArray[x] == z) {
        match = true;
      }
    }
    if (match == false) {

      //check longest histories for uniqueness
      //of transitions, if unique, don't delete
      transTemp = transTable->WhichStrings(z - removeAdjust);
      while (transTemp && isUnique == false) {
        //go to state of transitioning, max length history
        //and check it against other histories
        isUnique = CheckUniqueTrans(transTemp->stringPtr,
                                    z - removeAdjust,
                                    transTemp->state - removeAdjust,
                                    alpha);
        transTemp = transTemp->nextPtr;
      }

      //remove state only if state doesn't have a max length history
      //with unique characteristics transitiioning to it
      if (isUnique == false) {
        //reset transition table
        transTable->RemoveStringsAndState(z - removeAdjust, removeAdjust, lowest);
        //remove strings in state
        tempString = m_StateArray[z - removeAdjust]->getStringList();
        while (tempString) {
          tempString2 = tempString;
          tempString = tempString->m_nextPtr;
          m_table->RemoveString(tempString2->m_string);
        }
        //remove state itself
        //(also removes from hash table)
        RemoveState(z - removeAdjust);
        if (removeAdjust == 0) {
          lowest = z;
        }
        //restructure the state numbers after removal
        removeAdjust++;
        done = false;
      }
    }
  }
  return done;
}


///////////////////////////////////////////////////////////////////////////
//Function: AllStates::CreateChildStateArray
// Purpose: fills array of strings and array of integers which hold
//          information about the placement of one state's 'child' strings
//	    (which states they transition to)
// In Params: parse tree of strings, size of array, index of state to check
// Out Params: none
// In/Out Params: pointer to arrays to fill (one of strings and one of integers
// Pre- Cond: memory is allocated for arrays
// Post-Cond: arrays contain valid information
///////////////////////////////////////////////////////////////////////////
void AllStates::CreateChildStateArray(ParseTree &parsetree, int arraySize,
                                      int **stateArray,
                                      StringElem **stringArray, int index) {
  char *alpha = parsetree.getAlpha();
  int stringCounter = 0;
  StringElem *temp = m_StateArray[index]->getStringList();
  int length;
  char *childString = NULL;
  int childState;
  char *symbol = new char[2];
  int maxLength = parsetree.getMaxLength();
  bool isEmpty = false;

  //fill the arrays for each string
  while (temp != NULL) {
    //get string which is in state
    length = strlen(temp->m_string);

    stringArray[stringCounter] = new StringElem(*temp);

    //look at transitions of child strings
    for (int k = 0; k < m_distSize; k++) {
      childString = new char[length + 2 * SYMB_SIZE + END_STRING];
      strcpy(childString, temp->m_string);

      //wrap string if it is maxLength
      if (length == maxLength) {
        childString++;
      }

      symbol[0] = alpha[k];
      symbol[1] = '\0';

      //create child string
      strcat(childString, symbol);

      //determine state of child string
      childState = m_table->WhichStateNumber(childString);
      stateArray[k][stringCounter] = childState;
      if (length == maxLength) {
        childString--;
      }

      delete[] childString;
      childString = NULL;
    }
    stringCounter++;
    temp = temp->m_nextPtr;

  }
  delete[] symbol;
  return;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::CheckUniqueTrans
// Purpose: checks for special case where a max length history,
//          transitioning to a state which is otherwise transient,
//          has unique transitions when compared to other histories
// In Params: history which transitions to othersise transient state,
//            the otherwise transient state, the parent state of the
//            history, and the alphabet
// Out Params: boolean to denote whether history's transitions are unique
// In/Out Params: none
// Pre- Cond: History which transitions to bad state has been selected.
// Post-Cond: History is identified as unique
//////////////////////////////////////////////////////////////////////////
bool AllStates::CheckUniqueTrans(StringElem *transString, int removalState,
                                 int parentState, char *alpha) {
  bool isUnique = true;
  bool isMatch = true;
  int *transList = new int[m_distSize];
  int tempState = NULL_STATE;
  int histLength = strlen(transString->m_string);
  int maxLength = histLength;
  char *childHist = NULL;
  char *childHist2 = NULL;
  char *symbol = new char[2];
  symbol[1] = '\0';
  StringElem *temp = m_StateArray[parentState]->getStringList();
  int alphaCounter;

  //check to see what the target history transitions are
  for (alphaCounter = 0; alphaCounter < m_distSize; alphaCounter++) {
    childHist2 = new char[histLength + 2 * SYMB_SIZE + END_STRING];
    strcpy(childHist2, transString->m_string);
    childHist2++;
    symbol[0] = alpha[alphaCounter];
    strcat(childHist2, symbol);
    tempState = m_table->WhichStateNumber(childHist2);
    //if state is going to be removed, use wild card
    if (tempState == removalState) {
      transList[alphaCounter] = NULL_STATE;
    }
    else {
      transList[alphaCounter] = tempState;
    }
    childHist2--;
    delete[] childHist2;
    childHist2 = NULL;
  }
  //check every other (shorter) history's transitions
  histLength = strlen(temp->m_string);
  //if it is a self loop, discount it
  if (removalState == parentState) {
    isUnique = false;
  }
  while (temp && histLength < maxLength && isUnique == true) {
    //don't check string against self
    if (strcmp(temp->m_string, transString->m_string) != 0) {
      isMatch = true;
      alphaCounter = 0;
      childHist = new char[histLength + SYMB_SIZE + END_STRING];
      strcpy(childHist, temp->m_string);
      //check transition for each symbol
      while (isMatch == true && alphaCounter < m_distSize) {
        symbol[0] = alpha[alphaCounter];
        childHist2 = new char[histLength + SYMB_SIZE + END_STRING];
        strcpy(childHist2, childHist);
        strcat(childHist2, symbol);
        //set boolean if single transition fails to match
        if (transList[alphaCounter] != NULL_STATE) {
          tempState = m_table->WhichStateNumber(childHist2);

          if (tempState != transList[alphaCounter]
              && tempState != NULL_STATE) {
            isMatch = false;
          }
        }
        delete[] childHist2;
        alphaCounter++;
      }
      //set boolean if histories are similar
      if (isMatch == true) {
        isUnique = false;
      }
      delete[] childHist;
      childHist = NULL;
    }
    temp = temp->m_nextPtr;
    histLength = strlen(temp->m_string);
  }
  delete[] transList;
  delete[] symbol;
  return isUnique;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::InitialFrequencies
// Purpose: calculates the frequency of occurrence of each element in
//          alphabet
// In Params: input to program (alphabet and time-series)
// Out Params: any new states created
// In/Out Params: the alphabet and data strings
// Pre- Cond: Alphabet and Data arrays are set
// Post-Cond: All strings of length one have been examined, initial states
//            have been created
//////////////////////////////////////////////////////////////////////////
void AllStates::InitialFrequencies(ParseTree &parsetree) {
  G_Array g_array;
  StringElem *temp;
  char *charArray = new char[m_distSize];
  int *intArray = new int[m_distSize];

  //find strings of length one and their distributions
  int size = parsetree.FindRoots(charArray, intArray);

  //make first, NULL, state with unconditional
  //frequencies for one and zero as string counts
  temp = new StringElem(m_distSize);
  temp->m_string = new char[5];
  strcpy(temp->m_string, "NULL");
  for (int k = 0; k < m_distSize; k++) {
    temp->m_counts[k] = intArray[k];
  }

  //add first state to array of states and
  //calculate distributions
  Insert(temp, m_arraySize);
  m_StateArray[m_arraySize - 1]->CalcCurrentDist();
  delete temp;
  return;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::RemoveState
// Purpose: removes an empty state from the array of states
// In Params: index of state to remove
// Out Params: none
// In/Out Params: the array of states, the size of the array
// Pre- Cond: state is empty
// Post-Cond: state is removed from array, array size is one smaller
//////////////////////////////////////////////////////////////////////////
void AllStates::RemoveState(int index) {
  delete m_StateArray[index];
  for (int i = index; i < m_arraySize - 1; i++) {
    m_StateArray[i] = m_StateArray[i + 1];
    m_StateArray[i]->decrementNumber();
  }
  m_arraySize--;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::GetStateDistsMulti
// Purpose: calculates the distribution of each state, for multi-line input
// In Params: the data, the alphabet, the size of the data, the max length
//            of strings in the parse tree, the name of the data file
// Out Params: none
// In/Out Params: the array of states
// Pre- Cond: state transitions have been determined
// Post-Cond: state distributions have been determined
//////////////////////////////////////////////////////////////////////////
void AllStates::GetStateDistsMulti(ParseTree &parsetree, char input[],
                                   HashTable2 *hashtable, bool isMulti) {
  char *data = parsetree.getData();
  int dataLength = strlen(data);
  int adjustedDataLength = parsetree.getDataSize();
  // We need to deduct all the data-points where we're synchronizing from the
  // total, so we get probabilities for states which sum to one
  int numberLines = parsetree.getNumLines();
  int maxLength = parsetree.getMaxLength();
  int i = 0;
  int k = 0;
  int state = NULL_STATE;
  int *counts = new int[m_arraySize];
  char *symbol = new char[2];
  char *inputFile = new char[strlen(input) + 13 + END_STRING];
  int diff = 0;
  char *synchString0 = new char[MAX_STRING + 2 + END_STRING];

  //create file streams
  strcpy(inputFile, input);
  strcat(inputFile, "_state_series");
  ofstream outData(inputFile, ios::out);

  //open output file, if unsuccessful set boolean return value
  if (!outData) {
    cerr << " the state series output file cannot be opened " << endl;
    exit(1);
  }
    //otherwise output data
  else {
    //initializations
    for (int j = 0; j < m_arraySize; j++) {
      counts[j] = 0;
    }

    symbol[1] = '\0';

    //initial synchronization
    state = SynchToStatesMulti(i, k, state, maxLength, &outData, data,
                               dataLength, synchString0);

    //Adjust for synch time, check for end of line
    if (CheckSynchError(i, state, synchString0, parsetree, isMulti, i - 1)) {
      state = SynchToStatesMulti(i, k, state, maxLength, &outData, data,
                                 dataLength, synchString0);
    }

    adjustedDataLength -= (i - 1);
    symbol[0] = data[i];
    counts[state]++;
    outData << state << ";";

    //get series of states, and make frequency counts
    while (i < dataLength) {
      //end of line in data, resynchronize
      if (data[i] == '\n') {
        i = i + SYSTEM_CONST;
        //if the last return was the last character altogether
        if (i >= dataLength) {
          break;
        }
        k = 0;
        outData << '\n';
        diff = i;
        state = SynchToStatesMulti(i, k, state, maxLength, &outData, data,
                                   dataLength, synchString0);

        //Adjust for synch time, check for end of line
        if (CheckSynchError(i, state, synchString0, parsetree, isMulti,
                            i - diff - 1)) {
          state = SynchToStatesMulti(i, k, state, maxLength, &outData,
                                     data, dataLength, synchString0);
        }

        diff = i - diff - 1;
        adjustedDataLength -= diff;
        counts[state]++;
        outData << state << ";";
        symbol[0] = data[i];
      }

      state = m_StateArray[state]->getTransitions
          (hashtable->WhichIndex(symbol));

      //null transition, bad model, must resynchronize
      if (state == NULL_STATE) {
        diff = i;
        m_reSynch = true;
        state = SynchToStatesMulti(i, k, state, maxLength, &outData, data,
                                   dataLength, synchString0);


        //Adjust for synch time, check for end of line
        if (CheckSynchError(i, state, synchString0, parsetree, isMulti,
                            i - diff - 1)) {
          state = SynchToStatesMulti(i, k, state, maxLength, &outData,
                                     data, dataLength, synchString0);
        }

        i--;
        diff = i - diff - 1;
        adjustedDataLength -= diff;
      }
      counts[state]++;
      outData << state << ";";
      i++;
      symbol[0] = data[i];
    }
    outData.close();

    for (int q = 0; q < m_arraySize; q++) {
      m_StateArray[q]->setFrequency((double) counts[q] / (double)
          adjustedDataLength);
    }
  }

  delete[] counts;
  delete[] symbol;
  delete[] inputFile;
  delete[] synchString0;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::CheckSynchError
// Purpose: checks to see whether data is synchronized yet, and removes
//          unsynchronized data from parse tree
// In Params: index of data, current state determined, boolean for multi-line mode
// Out Params: a boolean representing whether more synchronizing is needed
// In/Out Params: the string of unsynchronized data, parsetree
// Pre- Cond: unsynched data is counted in parse tree, not known whether more
//            synchronization is needed
// Post-Cond: data is no longer counted in parse tree, have determined
//            whehter more synchronization is needed
//////////////////////////////////////////////////////////////////////////
bool AllStates::CheckSynchError(int index, int state, char *&synchString0,
                                ParseTree &parsetree, bool isMulti, int diff) {

  bool isSynchAgain = false;
  int dataSize = parsetree.getDataSize();

  //if it took longer than zero steps (data points) to synchronize
  if (diff > 0) {
    //if state is still null here, something is VERY wrong
    if (state == NULL_STATE && ((index >= dataSize - 1) || (!isMulti))) {
      cerr << "\nError: Could not synchronize, cannot use this data. "
      << endl
      << "AllStates::GetStateDistsMulti\n" << endl
      << "Note: For some reason this program cannot synchronize to a"
      << "state at any point in the data.  "
      << "See 'ReadMe' for details.\n"
      << endl;
      exit(1);
    }
      //if end of line reached before synched
    else if (state == NULL_STATE && isMulti) {
      //call synch to states again
      isSynchAgain = true;
    }
    //decrement synchString from tree
    parsetree.MakeSynchAdjustements(synchString0, index);
  }
  //clear synchstring
  delete[] synchString0;
  synchString0 = NULL;
  synchString0 = new char[MAX_STRING];

  return isSynchAgain;
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates::SynchtoStatesMulti
// Purpose: steps through data and synchronizes to a state in the inferred
//          machine
// In Params: the max length of histories in the parse tree, the name of the
//            data file to write the state series into, the length of the data
// Out Params: first state synched to
// In/Out Params: the index/place in the data, the index/place in a given line
// Pre- Cond: state sequence is unsynched
// Post-Cond: state sequence is synched
//////////////////////////////////////////////////////////////////////////
int AllStates::SynchToStatesMulti(int &index, int &lineIndex, int state,
                                  int maxLength, ofstream *outData, char *data,
                                  int dataLength, char *&synchString) {
  char *string = new char[maxLength + 1 + END_STRING];
  char *tempString;
  int tempIndex = 0;
  int synchCount = 0;
  char *symbol = new char[2];
  char *tempSynch;
  symbol[0] = data[index];
  int tempMax = MAX_STRING - 1;

  symbol[1] = '\0';
  //synchString[1] = '\0';

  strcpy(string, symbol);

  //synchronize to states
  do {
    if (lineIndex > 0) {
      *outData << "?;";
    }

    state = m_table->WhichStateNumber(string);

    if (state == NULL_STATE && tempIndex == 0) {
      strcpy(synchString, symbol);
    }
    else if (state == NULL_STATE) {
      strcat(synchString, symbol);
    }

    index++;
    lineIndex++;
    tempIndex++;
    symbol[0] = data[index];

    if (data[index] == '\n') {
      lineIndex = 0;
      tempIndex = 0;
      index = index + SYSTEM_CONST;
      *outData << '\n';
      break;
    }

    if (tempIndex >= maxLength) {
      tempString = new char[maxLength + 1 + END_STRING];
      string++;
      strcpy(tempString, string);
      string--;
      delete[] string;
      string = tempString;
    }

    if (tempIndex >= tempMax) {

      tempMax += MAX_STRING - 1;
      tempSynch = new char[tempMax + END_STRING];
      strcpy(tempSynch, synchString);
      delete[] synchString;
      synchString = tempSynch;
      tempSynch = NULL;
    }
    strcat(string, symbol);
  } while (state == NULL_STATE && index < dataLength);

  delete[] symbol;
  delete[] string;
  return state;
}


///////////////////////////////////////////////////////////////////////////
// Function: ~AllStates
// Purpose: destructor for AllStates
///////////////////////////////////////////////////////////////////////////
AllStates::~AllStates() {
  if (m_StateArray) {
    for (int i = 0; i < m_arraySize; i++) {
      delete m_StateArray[i];
      m_StateArray[i] = NULL;
    }
    delete[] m_StateArray;
  }
  if (m_table) {
    delete m_table;
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: AllStates
// Purpose: constructor for AllStates
///////////////////////////////////////////////////////////////////////////
AllStates::AllStates(int distSize, double sigLevel, bool isChi) {
  m_reSynch = false;
  m_sigLevel = sigLevel;
  m_test = new Test(isChi);
  m_distSize = distSize;
  m_arraySize = 0;
  m_maxArraySize = INITIAL_SIZE;
  m_StateArray = new State *[INITIAL_SIZE];
  for (int i = 0; i < INITIAL_SIZE; i++) {
    m_StateArray[i] = NULL;
  }
  m_table = new HashTable;
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
double AllStates::Compare(int k, double newDist[], int newDistCount) {
  double * currentDist = m_StateArray[k]->getCurrentDist();
  int distCount = m_StateArray[k]->getCount();

  return m_test->RunTest(currentDist, distCount, newDist, newDistCount, m_distSize);
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
double AllStates::Compare(State *state, double newDist[], int count) {
  return m_test->RunTest(state->getCurrentDist(), state->getCount(), newDist,
                         count, m_distSize);
}











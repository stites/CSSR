/////////////////////////////////////////////////////////////////////////////
//Title:	States.cpp
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:  Class for state object.  The state object represents a 
//              causal state, which contains all histories which 
//              determine the causal state. For use with CSSR.
//
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
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
#include "States.h"


///////////////////////////////////////////////////////////////////////////
// Function: State::Insert
// Purpose: adds a new string to the linked list in a state
// In Params: string for state
// Out Params: any new lists created
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: state has been initialized
// Post-Cond: string has been added to state
//////////////////////////////////////////////////////////////////////////
void State::Insert(char string[], HashTable *table) {
  //create temporary String Element
  StringElem *temp = new StringElem(m_distributionSize);
  temp->m_string = new char[strlen(string) + 1];

  //copy new string into String Element
  strcpy(temp->m_string, string);

  //enter in hash table
  table->Insert(temp, this);

  //if first in list, set up head and tail pointers
  if (m_listSize == 0) {
    m_StringList = m_listTail = temp;
    m_listSize++;
  }
    //otherwise put new element last in list
  else {
    m_listTail->m_nextPtr = temp;
    m_listTail = temp;
    temp = NULL;
    m_listSize++;
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: State::Insert
// Purpose: adds a new string and 'counts array' to the linked list in a state
// In Params: ArrayElem holding string and counts
// Out Params: any new lists created
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: state has been initialized
// Post-Cond: string has been added to state
//////////////////////////////////////////////////////////////////////////
void State::Insert(ArrayElem *elem, HashTable *table) {
  char *string = elem->getString();

  //create temporary String Element
  StringElem *temp = new StringElem(m_distributionSize);
  temp->m_string = new char[strlen(string) + 1];

  //copy new string into String Element
  strcpy(temp->m_string, string);

  //copy new counts into String Element
  for (int i = 0; i < m_distributionSize; i++) {
    temp->m_counts[i] = elem->getCounts()[i];
  }

  //enter in hash table
  table->Insert(temp, this);

  //if first in list, set up head and tail pointers
  if (m_listSize == 0) {
    m_StringList = m_listTail = temp;
    m_listSize++;
  }
    //otherwise put new element last in list
  else {
    m_listTail->m_nextPtr = temp;
    m_listTail = temp;
    temp = NULL;
    m_listSize++;
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: State::Insert
// Purpose: adds a new string and 'counts array' to the linked list in a state
// In Params: String Element to copy and enter
// Out Params: any new lists created
// In/Out Params: hash table of pointers to strings and states
// Pre- Cond: state has been initialized
// Post-Cond: string has been added to state
//////////////////////////////////////////////////////////////////////////
void State::Insert(StringElem *elem, HashTable *table) {
  //create temporary String Element
  StringElem *temp = new StringElem(*elem);

  //enter in hash table
  table->Insert(temp, this);

  //if first in list, set up head and tail pointers
  if (m_listSize == 0) {
    m_StringList = m_listTail = temp;
    m_listSize++;
  }
    //otherwise put new element last in list
  else {
    m_listTail->m_nextPtr = temp;
    m_listTail = temp;
    temp = NULL;
    m_listSize++;
  }
}


///////////////////////////////////////////////////////////////////////////
// Function: State::CalcCurrentDist
// Purpose: calculates the frequency of occurrence of each element given
//          state - P(element|state)
// In Params: input to program (alphabet and time-series)
// Out Params: any new states created
// In/Out Params: the alphabet and data strings
// Pre- Cond: Alphabet and Data arrays are set, all strings of length one
//            have been examined, initial states  have been created
// Post-Cond: State has array of probabilities of occurence for each
//            element in the alphabet
//////////////////////////////////////////////////////////////////////////
void State::CalcCurrentDist() {
  //points to current string
  StringElem *stringElem;

  //initialize the occurence count for all strings in state
  m_occurenceCount = 0;

  //set temporary to point to first string
  stringElem = m_StringList;

  //initialize distribution
  for (int j = 0; j < m_distributionSize; j++) {
    m_currentDist[j] = 0;
  }

  // iterate through each of the "histories" in the state
  while (stringElem != NULL) {
    // for each of these histories
    for (int i = 0; i < m_distributionSize; i++) {
      // get the total counts for the history
      m_occurenceCount += stringElem->m_counts[i];
      // and start aggregating these counts for their
      // respective symbol in the full state
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
}


///////////////////////////////////////////////////////////////////////////
// Function: ~State
// Purpose: destructor for State Class
///////////////////////////////////////////////////////////////////////////
State::~State() {
  StringElem *temp1 = m_StringList;
  StringElem *temp2;

  while (temp1 != m_listTail) {
    temp2 = temp1->m_nextPtr;
    delete temp1;
    temp1 = temp2;
  }
  if (m_listTail) { delete m_listTail; }
  if (m_currentDist != NULL) { delete[] m_currentDist; }
}


///////////////////////////////////////////////////////////////////////////
//Function: PrintStringList
//Purpose: prints out linked list of strings stored in state
///////////////////////////////////////////////////////////////////////////
void State::PrintStringList(ofstream *outData, char alpha[]) {
  StringElem *temp1 = m_StringList;
  char null[6] = "NULL\0";

  if (temp1 != NULL) {
    //print out strings
    while (temp1 != m_listTail) {
      *outData << temp1->m_string << endl;
      temp1 = temp1->m_nextPtr;
    }
    *outData << temp1->m_string << endl;
    *outData << "distribution: ";

    //print out distributions and transitions
    for (int i = 0; i < m_distributionSize; i++) {
      *outData << "P(" << alpha[i] << ") = " << m_currentDist[i] << "\t";
    }
    *outData << endl << "transitions: ";

    for (int k = 0; k < m_distributionSize; k++) {
      if (m_transitions[k] == -1) {
        *outData << "T(" << alpha[k] << ") = " << null << "\t";
      }
      else {
        *outData << "T(" << alpha[k] << ") = " << m_transitions[k] << "\t";
      }
    }

    *outData << endl;
    *outData << "P(state): " << m_frequency;
  }
  else {
    *outData << "empty state" << endl;
  }

  *outData << endl << endl;
}


///////////////////////////////////////////////////////////////////////////
// Function: State
// Purpose: constructor for State Class
///////////////////////////////////////////////////////////////////////////
State::State(int distSize, int number) {
  m_distributionSize = distSize;
  m_number = number;
  m_listSize = 0;
  m_listTail = NULL;
  m_StringList = NULL;
  m_occurenceCount = 0;
  m_currentDist = new double[distSize];
  m_transitions = new int[distSize];
}


///////////////////////////////////////////////////////////////////////////
// Function: StringElem
// Purpose: constructor for StringElem Class
///////////////////////////////////////////////////////////////////////////
StringElem::StringElem(int distSize) {
  m_size = distSize;
  m_counts = new int[distSize];
  m_string = NULL;
  m_nextPtr = NULL;
}


///////////////////////////////////////////////////////////////////////////
// Function: State::RemoveString
// Purpose: deletes an element from the list
// In Params: element to be removed
// Out Params: none
// In/Out Params: none
// Pre- Cond: new state has been generated by an extension of string to 
//            be deleted, and all other extensions have been tested
// Post-Cond: element/string has been deleted from list/state
///////////////////////////////////////////////////////////////////////////    
void State::RemoveString(StringElem *element) {
  StringElem *temp;
  StringElem *temp2;

  if (element) {
    if (element == m_StringList) {
      temp = m_StringList;
      if (m_listTail == m_StringList) {
        m_listTail = m_StringList = NULL;
      }
      else {
        m_StringList = m_StringList->m_nextPtr;
      }
      delete temp;
    }
    else {
      for (temp = m_StringList;
           temp->m_nextPtr != NULL && temp->m_nextPtr != element;
           temp = temp->m_nextPtr) { }
      if (temp->m_nextPtr == NULL) {
        cerr << "trying to remove a string which does not exist in"
        << " state";
        exit(1);
      }
      if (temp->m_nextPtr == m_listTail) {
        m_listTail = temp;
        delete temp->m_nextPtr;
        temp->m_nextPtr = NULL;
      }
      else {
        temp2 = temp->m_nextPtr->m_nextPtr;
        delete temp->m_nextPtr;
        temp->m_nextPtr = temp2;
      }
    }
    m_listSize--;
  }
}

std::string State::toString() {
  StringElem* temp = m_StringList;
  std::string tempStr;
  while (temp != NULL) {
    std::string delim =  (temp->m_nextPtr == NULL) ? "" : ", ";
    tempStr += (temp->getString()) + delim;
    temp = temp->m_nextPtr;
  }
  return "State { size: "+ std::to_string(m_listSize) + " ["+ tempStr +"] }";
}

///////////////////////////////////////////////////////////////////////////
// Function: StringElem
// Purpose: copy constructor for StringElem struct
///////////////////////////////////////////////////////////////////////////    
StringElem::StringElem(const StringElem &oldElem) {
  m_nextPtr = NULL;
  m_size = oldElem.m_size;

  //allocate space for string and count info
  m_string = new char[strlen(oldElem.m_string) + 1];
  strcpy(m_string, oldElem.m_string);
  m_counts = new int[m_size];

  for (int i = 0; i < m_size; i++) {
    m_counts[i] = oldElem.m_counts[i];
  }

  return;
}

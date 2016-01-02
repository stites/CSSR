///////////////////////////////////////////////////////////////////////////////
// Title	:	TransTable
// Date		:	March 20, 2002
// Author	:	Kristina Klinkner
// Description	: creates a table of transitions from all max length strings
//                for use with CSSR
///////////////////////////////////////////////////////////////////////////////

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

#include "TransTable.h"

///////////////////////////////////////////////////////////////////////////////
TransTable::TransTable(int numStates) {
  m_transArray = new TransNode *[numStates];
  m_parentArray = new TransNode *[numStates];
  m_transCounts = new int[numStates];

  for (int i = 0; i < numStates; i++) {
    m_transArray[i] = NULL;
    m_parentArray[i] = NULL;
    m_transCounts[i] = 0;
  }
  m_arraySize = numStates;
}


TransTable::~TransTable() {
  TransNode *temp;
  TransNode *temp2;
  delete[] m_transCounts;

  //delete lists
  for (int i = 0; i < m_arraySize; i++) {
    if (m_transArray[i]) {
      temp = m_transArray[i]->nextPtr;
    }
    else {
      temp = NULL;
    }
    while (temp) {
      temp->stringPtr = NULL;
      temp2 = temp->nextPtr;
      delete temp;
      temp = temp2;
    }
  }

  //delete lists
  for (int k = 0; k < m_arraySize; k++) {
    if (m_parentArray[k]) {
      temp = m_parentArray[k]->nextPtr;
    }
    else {
      temp = NULL;
    }
    while (temp) {
      temp->stringPtr = NULL;
      temp2 = temp->nextPtr;
      delete temp;
      temp = temp2;
    }
  }

  //delete all initial pointers
  delete[] m_transArray;
  delete[] m_parentArray;
}


void TransTable::setTrans(int transState, StringElem *string, int parentState) {
  if (transState == NULL_STATE) {
    return;
  }
  else {
    //create a new TransNode and put it at the front of the list
    if ((transState >= 0) && (transState < m_arraySize)) {
      TransNode *temp1 = new TransNode;
      TransNode *temp3 = new TransNode;
      if (temp1 == NULL || temp3 == NULL) {
        cerr << "Out of memory." << endl;
        exit(1);
      }
      //set array which finds by transition state
      temp1->stringPtr = string;
      temp1->state = parentState;
      temp1->nextPtr = NULL;

      TransNode *temp2;
      temp2 = m_transArray[transState];
      m_transArray[transState] = temp1;
      temp1->nextPtr = temp2;

      //set array which finds by parent state
      temp2 = NULL;
      temp3->stringPtr = string;
      temp3->state = transState;
      temp3->nextPtr = NULL;

      temp2 = m_parentArray[parentState];
      m_parentArray[parentState] = temp3;
      temp3->nextPtr = temp2;

      //set counters
      m_transCounts[transState]++;
    }
    else {
      cerr << "TransTable::setTrans::Invalid state " << endl;
    }
  }
}


void TransTable::RemoveStringsAndState(int removalState, int removeAdjust, int lowest) {
  TransNode *removeTemp;
  TransNode *removeTemp2;
  TransNode *ptrTemp1;
  TransNode *ptrTemp2;
  int transState = NULL_STATE;
  int i = 0;

  //get rid of nodes which transition from bad state

  //first node which is an element of a bad state
  removeTemp = m_parentArray[removalState];

  while (removeTemp) {
    //transition entry containing that node
    transState = removeTemp->state;
    if (transState > lowest) {
      transState = transState - removeAdjust;
    }
    ptrTemp1 = m_transArray[transState];
    if (ptrTemp1) {
      //if the matching node is the first in the entry remove it
      if (ptrTemp1->stringPtr == removeTemp->stringPtr) {
        m_transArray[transState] = ptrTemp1->nextPtr;
        ptrTemp1->stringPtr = NULL;
        delete ptrTemp1;
      }
      else {
        //if not, find it
        while (ptrTemp1->stringPtr != removeTemp->stringPtr) {
          ptrTemp2 = ptrTemp1;
          ptrTemp1 = ptrTemp1->nextPtr;
        }
        //remove node while keeping linked list together
        ptrTemp2->nextPtr = ptrTemp1->nextPtr;
        ptrTemp1->stringPtr = NULL;
        delete ptrTemp1;
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

  //lastly, remove the  bad state from both arrays
  for (i = removalState; i < m_arraySize - 1; i++) {
    m_parentArray[i] = m_parentArray[i + 1];
    m_transArray[i] = m_transArray[i + 1];
  }
  m_parentArray[i] = NULL;
  m_transArray[i] = NULL;
  m_arraySize--;
}


int TransTable::getCounts(int state) {
  return m_transCounts[state];
}


TransNode *TransTable::WhichStrings(int state) {
  return m_transArray[state];
}

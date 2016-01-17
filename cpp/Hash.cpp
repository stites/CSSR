///////////////////////////////////////////////////////////////////////////////
// Title		:	Hash
// Date			:	March 20, 2002
// Author		:	Kristina Klinkner
// Description	:	creates a hash table of pointers to strings and their
//                      states for use with CSSR
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

#include "Hash.h"

///////////////////////////////////////////////////////////////////////////////


HashTable::HashTable() {
  for (int i = 0; i < HASHSIZE; i++) {
    m_data[i] = NULL;
  }
}


HashTable::~HashTable() {
  for (int i = 0; i < HASHSIZE; i++) {
    if (m_data[i] != NULL) {
      HashTableEntry *temp1;
      HashTableEntry *temp2 = m_data[i]->m_nextPtr;
      while (temp2 != NULL) {
        temp1 = temp2;
        temp2 = temp2->m_nextPtr;
        //delete hash entries in list
        delete temp1;
        temp1 = NULL;
      }
      //delete node for main hash entry
      delete m_data[i];
      m_data[i] = NULL;
    }
  }
}


int HashTable::Hash(ulong key) {
  int hashValue = key % HASHSIZE;
  return hashValue;
}


////////////////////////////////////////////////////////////////
//Function: HashTable::Insert
//Purpose: inserts a new element in the hash table, if there is
//		   already an element at the appropriate index, puts new
//		   element at the front of the list. 
//In parameter: new element and it's parent state
////////////////////////////////////////////////////////////////
void HashTable::Insert(StringElem *elem, State *state) {
  if (elem == NULL) {
    cerr << "Cannot insert null pointer into Hash Table\n";
    exit(1);
  }

  ulong key = CreateKey(elem->m_string);
  int hashValue = Hash(key);

  //create a new HashTableEntry and put it at the front of the list
  if ((hashValue >= 0) && (hashValue < HASHSIZE)) {
    HashTableEntry *temp1 = new HashTableEntry;
    if (temp1 == NULL) {
      cerr << "Out of memory." << endl;
      exit(1);
    }

    temp1->m_stringPtr = elem;
    temp1->m_statePtr = state;
    HashTableEntry *temp2;
    temp2 = m_data[hashValue];
    m_data[hashValue] = temp1;
    temp1->m_nextPtr = temp2;
  }
  else {
    cerr << "Invalid hash value " << endl;
  }
}


/////////////////////////////////////////////////////////////////
//Function: Hash::Which State
//Purpose: checks to see which state string is in; returns
//         a pointer to the state
//In parameter: string to check
//Return value: pointer to address of appropriate state
////////////////////////////////////////////////////////////////
State *HashTable::WhichState(char *string) {
  if (string == '\0') {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  ulong currentKey = CreateKey(string);
  int hashValue = Hash(currentKey);
  State *statePtr = NULL;

  //traverse list
  HashTableEntry *temp = m_data[hashValue];
  while ((temp != NULL) && (statePtr == NULL)) {
    if (strcmp(string, (temp->m_stringPtr->m_string)) == 0) {
      statePtr = temp->m_statePtr;
    }

    temp = temp->m_nextPtr;
  }
  return statePtr;
}


/////////////////////////////////////////////////////////////////
//Function: Hash::Which StateNumber
//Purpose: checks to see which state string is in; returns the 
//         number of the state
//In parameter: string to check
//Return value: assigned number of appropriate state
////////////////////////////////////////////////////////////////
int HashTable::WhichStateNumber(char *string) {
  if (string == '\0') {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  ulong currentKey = CreateKey(string);
  int hashValue = Hash(currentKey);
  State *statePtr = NULL;

  //traverse list
  HashTableEntry *temp = m_data[hashValue];
  while ((temp != NULL) && (statePtr == NULL)) {
    if (strcmp(string, (temp->m_stringPtr->m_string)) == 0) {
      statePtr = temp->m_statePtr;
    }
    temp = temp->m_nextPtr;
  }

  if (statePtr) {
    return statePtr->getNumber();
  }
  else {
    return NULL_STATE;
  }
}


ulong HashTable::CreateKey(char *string) {
  ulong key = 0;
  const char *ptr = string;

  while (*ptr) {
    key += (key << 5) + *ptr++;
  }

  return key;
}


void HashTable::Print() {
  HashTableEntry *temp;
  for (int i = 0; i < HASHSIZE; i++) {
    temp = m_data[i];
    while (temp != NULL) {
      cout << temp->m_stringPtr->m_string << endl;
      temp = temp->m_nextPtr;
    }
  }
}


void HashTable::RemoveString(char *string) {
  if (string == '\0') {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  ulong currentKey = CreateKey(string);
  int hashValue = Hash(currentKey);
  State *statePtr = NULL;
  StringElem *stringPtr = NULL;

  //traverse list
  HashTableEntry *temp = m_data[hashValue];
  HashTableEntry *temp2;
  while ((temp != NULL) && (strcmp(string, (temp->m_stringPtr->m_string)) != 0)) {
    temp2 = temp;
    temp = temp->m_nextPtr;
  }

  if (temp != NULL) {
    //when proper pointers have been found
    stringPtr = temp->m_stringPtr;
    statePtr = temp->m_statePtr;

    //remove string from state
    statePtr->RemoveString(stringPtr);

    //remove hash table entry
    if (temp == m_data[hashValue]) {
      m_data[hashValue] = temp->m_nextPtr;

    }
    else {
      temp2->m_nextPtr = temp->m_nextPtr;
    }


    //remove entry from table
    delete temp;
  }
  return;
}

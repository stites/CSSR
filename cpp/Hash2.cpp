///////////////////////////////////////////////////////////////////////////////
// Title		:	Hash2
// Date			:	March 20, 2002
// Author		:	Kristina Klinkner
// Description	:	creates a hash table of symbols and their index for use with 
//				    CSSR
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
#include "Hash2.h"

///////////////////////////////////////////////////////////////////////////////



HashTable2::HashTable2() {
  for (int i = 0; i < HASHSIZE2; i++) {
    m_data[i] = NULL;
  }
}


HashTable2::~HashTable2() {
  for (int i = 0; i < HASHSIZE2; i++) {
    if (m_data[i] != NULL) {
      HashTable2Entry *temp1;
      HashTable2Entry *temp2 = m_data[i]->m_nextPtr;
      while (temp2 != NULL) {
        temp1 = temp2;
        temp2 = temp2->m_nextPtr;
        //delete hash entries in list
        if (temp1->m_string) {
          delete temp1->m_string;
        }

        delete temp1;
        temp1 = NULL;
      }
      //delete node for main hash entry
      delete m_data[i];
      m_data[i] = NULL;
    }
  }
}


int HashTable2::Hash(ulong key) {
  int hashValue = key % HASHSIZE2;
  return hashValue;
}


////////////////////////////////////////////////////////////////
//Function: HashTable2::Insert
//Purpose: inserts a new element in the hash table, if there is
//		   already an element at the appropriate index, puts new
//		   element at the front of the list. 
//In parameter: new string and index
////////////////////////////////////////////////////////////////
void HashTable2::Insert(char *string, int index) {
  if (string == NULL) {
    cerr << "Cannot insert null pointer into Hash Table\n";
    exit(1);
  }
  char *tempstring = NULL;
  tempstring = new char[strlen(string) + 1];
  if (tempstring == NULL) {
    cerr << "Out of memory." << endl;
    exit(1);
  }

  strcpy(tempstring, string);
  ulong key = CreateKey(string);
  int hashValue = Hash(key);

  //create a new HashTable2Entry and put it at the front of the list
  if ((hashValue >= 0) && (hashValue < HASHSIZE2)) {
    HashTable2Entry *temp1 = NULL;
    temp1 = new HashTable2Entry;
    if (temp1 == NULL) {
      cerr << "Out of memory." << endl;
      exit(1);
    }
    temp1->m_string = tempstring;
    temp1->m_index = index;
    HashTable2Entry *temp2;
    temp2 = m_data[hashValue];
    m_data[hashValue] = temp1;
    temp1->m_nextPtr = temp2;
  }
  else {
    cerr << "Invalid hash value " << endl;
  }
}


/////////////////////////////////////////////////////////////////
//Function: HashTable2::WhichIndex
//Purpose: checks to see which state string is in; returns
//         a pointer to the state
//In parameter: string to check
//Return value: pointer to address of appropriate state
////////////////////////////////////////////////////////////////
int HashTable2::WhichIndex(char *string) {
  int index;
  if (string == '\0') {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  ulong currentKey = CreateKey(string);
  int hashValue = Hash(currentKey);
  index = -1;

  //traverse list
  HashTable2Entry *temp = m_data[hashValue];
  while ((temp != NULL) && (index == -1)) {
    if (strcmp(string, (temp->m_string)) == 0) {
      index = temp->m_index;
    }

    temp = temp->m_nextPtr;
  }

  if (index == -1) {
    cerr << "HashTable2::WhichIndex: String or symbol not in table.\n"
    << "A string/history has been encountered in the data which has"
    << "not been recorded in the set of states.  "
    << "See 'ReadMe' file for details";
    exit(1);
  }
  return index;
}


ulong HashTable2::CreateKey(char *string) {
  ulong key = 0;
  const char *ptr = string;
  while (*ptr) {
    key += (key << 5) + *ptr++;
  }

  return key;
}


void HashTable2::Print() {
  HashTable2Entry *temp;

  for (int i = 0; i < HASHSIZE2; i++) {
    temp = m_data[i];
    while (temp != NULL) {
      cout << temp->m_string << "\t" << temp->m_index << endl;
      temp = temp->m_nextPtr;
    }
  }
}

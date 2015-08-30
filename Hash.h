////////////////////////////////////////////////////////////////////////
//Title:	Hash.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for Hash.cpp
//
////////////////////////////////////////////////////////////////////////

#ifndef	__HASH_H
#define	__HASH_H
#define HASHSIZE  359

#include "Common.h"
#include "States.h"

//Forward declarations
class StringElem;
class State;

class HashTable {
 private:

  struct HashTableEntry {
    StringElem* m_stringPtr;
    State* m_statePtr; 
    HashTableEntry* m_nextPtr;
  };

  HashTableEntry* m_data[HASHSIZE];
  int Hash(ulong key);
  ulong CreateKey(char* string);

 public:

  HashTable();
  ~HashTable();
  void Insert(StringElem* elem, State* state);
  State* WhichState(char* string);
  int WhichStateNumber(char* string);
  void Print();
  void RemoveString(char* string);
};

#endif

////////////////////////////////////////////////////////////////////////
//Title:	Hash2.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for Hash2.cpp
//
////////////////////////////////////////////////////////////////////////

#ifndef	__HASH2_H
#define	__HASH2_H
#define HASHSIZE2  19

#include "Common.h"

class HashTable2 {
 private:

  struct HashTable2Entry {
    char* m_string;
    int m_index;
    HashTable2Entry* m_nextPtr;
  };

  HashTable2Entry* m_data[HASHSIZE2];
  int Hash(ulong key);
  ulong CreateKey(char* string);

 public:

  HashTable2();
  ~HashTable2();
  void Insert(char* string, int index);
  int WhichIndex(char* string);
  void Print();
};
#endif

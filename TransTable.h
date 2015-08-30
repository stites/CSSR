////////////////////////////////////////////////////////////////////////
//Title:	TransTable.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for TransTable.cpp
//
////////////////////////////////////////////////////////////////////////

#ifndef	__TRANS_H
#define	__TRANS_H

#include "Common.h"
#include "States.h"




struct TransNode
{
  StringElem* stringPtr;
  TransNode* nextPtr;
  int state;
};

class TransTable
{

 private:
  TransNode** m_transArray;
  TransNode** m_parentArray;
  int m_arraySize;
  int* m_transCounts;

public:
  TransTable(int numStates);
  ~TransTable();
  void setTrans(int transState, StringElem* string, int parentState);
  TransNode* WhichStrings(int state);
  int getCounts(int state);
  void RemoveStringsAndState(int removalState, int removeAdjust, int lowest);
};


#endif

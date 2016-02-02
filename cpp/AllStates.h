////////////////////////////////////////////////////////////////////////
//Title:	AllStates.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for AllStates.cpp
//
/////////////////////////////////////////////////////////////////////////////


#ifndef __ALLSTATES_H
#define __ALLSTATES_H

#include "States.h"
#include "Common.h"
#include "ParseTree.h"
#include "G_Array.h"
#include "Hash.h"
#include "Hash2.h"
#include "TransTable.h"
#include "Test.h"


//forward declarations
class State;

class StringElem;

class Test;


class AllStates {
public:
    AllStates(int distSize, double sigLevel, bool isChi);

    ~AllStates();

    void Insert(char string[], int index);

    void Insert(ArrayElem *elem, int index);

    void Insert(StringElem *elem, int index);

    void Insert(ArrayElem *elem, State *state);

    void InitialFrequencies(ParseTree &parsetree);

    bool Is_Full() { return (m_arraySize == m_maxArraySize - 1); }

    void Grow(int newsize);

    State *getState(int i) { return (m_StateArray[i]); }

    int getArraySize() { return m_arraySize; }

    int getDistSize() { return m_distSize; }

    void CalcNewDist(int length, ParseTree &parsetree);

    void PrintOut(char input[], char alpha[]);

    double Compare(int k, int j);

    double Compare(int k, double newDist[], int count);

    double Compare(State *state, double newDist[], int count);

    bool CompareToParent(char *&removalString, State *&removalState,
                         ArrayElem *element, double *newDist, int length,
                         bool match, int stringCount);

    HashTable *getTable() { return m_table; }

    void Determinize(ParseTree &parsetree);

    void CreateChildStateArray(ParseTree &parsetree, int arraySize,
                               int **stateArray, StringElem **stringArray,
                               int index);

    void AllocateArray(int **stateArray, int arraySize);

    void DeAllocateArray(int **stateArray);

    void StoreTransitions(int maxLength, char *alpha);

    void RemoveState(int index);

    void RemoveAncestors(char *&removalString, State *&removalState);

    void GetStateDists(ParseTree &parsetree, char input[],
                       HashTable2 *hashtable);

    void GetStateDistsMulti(ParseTree &parsetree, char input[],
                            HashTable2 *hashtable, bool isMulti);

    void FindNSetTransitions(int state, int maxLength, char *alpha);

    bool DestroyLongHists(int maxLength, ParseTree &parsetree);

    bool DestroyShortHists(int maxLength, ParseTree &parsetree);

    void FindSimilarTransitions(StringElem **stringArray, int **stateArray,
                                int childState, int tempSize, int alphaIndex,
                                int stringIndex, int stringMax,
                                bool isFirstPass);

    bool MakeNewDetStates(int stateIndex, StringElem **stringArray,
                          int **stateArray, int stringMax, bool isDeterministic,
                          ParseTree &parsetree);

    bool RePartition(bool match, char *removalString, State *removalState,
                     ArrayElem *element, double *newDist, int stringCount);

    void CheckConnComponents(ParseTree &parsetree);

    bool RemoveTransientStates(int *stateArray, bool done, int stateCounter,
                               TransTable *transTable, char *alpha);

    void FillRecurrStateArray(char *alpha, int maxLength, int index,
                              int *&stateArray, int &stateCounter,
                              TransTable *&transTable);

    int SynchToStates(int &index, int state, int maxLength, ofstream outData,
                      char *data, int dataLength);

    int SynchToStatesMulti(int &index, int &lineIndex, int state, int maxLength,
                           ofstream *outData, char *data, int dataLength,
                           char *&synchString);

    bool getReSynch() { return m_reSynch; }

    bool CheckUniqueTrans(StringElem *transString, int removalState,
                          int parentState, char *alpha);

    bool CheckSynchError(int index, int state, char *&synchsString0,
                         ParseTree &parsetree, bool isMulti, int diff);
    std::string toString();

private:
    State **m_StateArray; //collection of pointers to all the states in a
    // growable array
    int m_arraySize;      //size of array
    int m_maxArraySize;   //the largest the array can be before it grows
    int m_distSize;       //the size of the distribution array
    HashTable *m_table;   //hash table to access string locations in constant
    //time
    double m_sigLevel;    //chance of splitting when we shouldn't
    bool m_reSynch;       //whether this machine needed to be resynchronized
    //during state series output
    Test *m_test;
};

#endif

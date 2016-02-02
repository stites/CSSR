////////////////////////////////////////////////////////////////////////
//Author:      Kristina Klinkner
//Date:        March 20, 2002
////////////////////////////////////////////////////////////////////////

#ifndef __STATES_H
#define __STATES_H

#include "Common.h"
#include "G_Array.h"
#include "Hash.h"

#define MAXLENGTH 5    // maximum length of string

// Forward declarations
class HashTable;

class StringElem;

class ArrayElem;

class State {
    friend class StringElem;

private:
    StringElem *m_StringList;   //linked list of strings in the state
    int m_listSize;             //size of list?????
    StringElem *m_listTail;     //last element in list
    double *m_currentDist;      //probability distributions for current state
    int m_occurenceCount;       //number of times current state's
    //strings occur in data
    int m_distributionSize;     //number of values in distribution
    int m_number;               //state number in array of states
    int *m_transitions;         //which states this state transitions to on a
    //given symbol
    double m_frequency;         //probability of seeing the state

public:
    State(int distSize, int number);

    ~State();

    void Insert(char string[], HashTable *table);

    void Insert(ArrayElem *elem, HashTable *table);

    void Insert(StringElem *elem, HashTable *table);

    StringElem *getStringList() { return m_StringList; }

    void CalcCurrentDist();

    double *getCurrentDist() { return m_currentDist; }

    void PrintStringList(ofstream *outData, char alpha[]);

    int getListSize() { return m_listSize; }

    int getCount() { return m_occurenceCount; }

    void RemoveString(StringElem *element);

    int getNumber() { return m_number; }

    void setCurrentDist(double dist, int index) { m_currentDist[index] = dist; }

    void setTransitions(int index, int childState) { m_transitions[index] = childState; }

    int getTransitions(int index) { return m_transitions[index]; }

    void setFrequency(double freq) { m_frequency = freq; }

    void decrementNumber() { m_number--; }

    double getFrequency() { return m_frequency; }

    std::string toString();
};

class StringElem {
public:
    char *m_string;

    std::string getString(){
      return std::string (m_string);
    }

    StringElem *m_nextPtr;

    int *m_counts;

    int m_size;

    StringElem(int distSize);

    StringElem(const StringElem &oldElem);

    ~StringElem() {
      if (m_string) {
        delete[] m_string;
        m_string = NULL;
      }
      if (m_counts != NULL) {
        delete[] m_counts;
        m_counts = NULL;
      }
    }
};

#endif


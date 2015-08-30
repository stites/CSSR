////////////////////////////////////////////////////////////////////////
//Title:	Machine.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for Machine.cpp
//
/////////////////////////////////////////////////////////////////////////////


#ifndef __MACHINE_H
#define __MACHINE_H

#include "States.h"
#include "AllStates.h"
#include "Common.h"
#include "G_Array.h"
#include "Hash2.h"
#include "Hash.h"

class Machine
{
 public:

  Machine(AllStates* allstates);
  ~Machine(){};
  void CalcStringProbs(G_Array* g_array, 
		       int maxLength, HashTable2* hashtable, 
		       double stringProbs[]);
  double CalcStringProb(char* string, HashTable2* hashtable);
  void CalcRelEnt(ParseTree& parsetree, HashTable2* hashtable, bool isMulti);
  double CalcRelEntRateHist(double* stringProbs, ArrayElem** list, 
			    HashTable2* hashtable,int index, char* alpha, 
			    int alphaSize, int adjustedDataSize);
  double CalcRelEntRateAlpha(double stringProb, char* history, 
			     double& accumulatedInferredRatio, double dataDist,
			     char alphaElem,  HashTable2* hashtable);
  void CalcRelEntRate(ParseTree& parsetree, HashTable2* hashtable,
		      bool isMulti);
  void CalcVariation(ParseTree& parsetree, HashTable2* hashtable,
		     bool isMulti);     
  void CalcCmu();                 
  void CalcEntRate();			  
  double getRelEnt(){return m_relEnt;}
  double getCMu(){return m_cMu;}
  double getEntRate(){return m_entRate;}
  void PrintOut(char input[], const char*, const char*, const int&, const double&, const bool&, const bool&, int);
  void PrintDot(char input[], char alpha[]);

 private:

  double m_relEnt;         //relative entropy of machine
  double m_variation;	   //distance measure for machine and data
  double m_cMu;            //statistical complexity of machine
  double m_entRate;        //entropy rate of machine
  double m_relEntRate;     //relative entropy per symbol
  AllStates* m_allstates;  //pointer to array of states
  
};

#endif

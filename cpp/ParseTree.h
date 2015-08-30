////////////////////////////////////////////////////////////////////////
//Title:	ParseTree.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for ParseTree.cpp
//
/////////////////////////////////////////////////////////////////////////////
#ifndef PARSE_H
#define PARSE_H



#include "Common.h"
#include "G_Array.h"
#include "Hash2.h"
#define MAX_LINE_SIZE 500000

class TreeNode
{
protected:

  TreeNode* m_child;
  TreeNode* m_sibling;
  char m_symbol;
  int m_count;

 public:

  TreeNode(){m_child = NULL; m_sibling = NULL; m_symbol = '\0'; m_count = 0;}
  ~TreeNode(){}
  
friend class ParseTree;

};


class ParseTree
{
public:
  	
  void ReadInput(char alphaFile[], char dataFile[]);
  void ReadProcessMultiLine(char alphaFile[], char dataFile[]);
  void Insert(char string[]){Insert(string,m_root);}
  ParseTree(int length){m_root = NULL; m_alpha = NULL;
                        m_alphaSize = 0;m_data = NULL; m_dataSize = 0; 
                        m_maxLength = length; m_alphaHash = new HashTable2;
                        m_numLines = 0; m_adjustedDataSize = 0;}
  ~ParseTree(){RemoveTree(m_root);if(m_alphaHash) delete m_alphaHash;}
  void FindStrings(int length, G_Array* array);
  int FindRoots(char charArray[],int intArray[])
                {return FindRoots(m_root, charArray, intArray);}
  void FillTree();
  int getAlphaSize(){return m_alphaSize;}
  char* getAlpha(){return m_alpha;}
  int getMaxLength(){return m_maxLength;}
  char* getData(){return m_data;}
  int getDataSize(){return m_dataSize;}
  int getNumLines(){return m_numLines;}
  HashTable2* MakeAlphaHash();
  int getAdjustedDataSize(){return m_adjustedDataSize;}
  void MakeSynchAdjustements(char* synchString, int index);
  void DecStringCount(char* stringToDec);

protected:

  void RemoveTree(TreeNode*& root);
  void Insert(char string[], TreeNode*& root);
  void FindStrings(TreeNode* root, int length, char* parentString,
		   G_Array* tempArray);
  int FindRoots(TreeNode* root, char charArray[], int intArray[]);
  void DecStringCount(char string[], TreeNode*& root);
  void GetDataInput(char dataFile[]);
  void GetAlphaInput(char alphaFile[]);
  void CheckAlphaInput();
  void CheckDataInput();

private:

  TreeNode* m_root;
  char* m_alpha;
  int m_alphaSize;
  char* m_data;
  int m_dataSize;
  int m_maxLength;
  HashTable2* m_alphaHash;
  int m_numLines;
  int m_adjustedDataSize;
};

#endif

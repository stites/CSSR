////////////////////////////////////////////////////////////////////////
//Title:	Common.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for use with all files in 'CSSR' program
//
////////////////////////////////////////////////////////////////////////
#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <fstream>
#include <stdio.h>
#include <iostream>
#include <iomanip>
#include <string.h>
#include <ctype.h>

#define INCREMENT 10     //size of array growth
#define INITIAL_SIZE 50  //intial size of array of states
#define NULL_STATE -1   //non state value
#define SYSTEM_CONST 1   //set this to 2 for windows, and 1 for unix
#define END_STRING 1     //symbol at end of string
#define SYMB_SIZE 1      //size of alpha symbol in ascii characters
#define MAX_STRING 80   //maximum memory allocation for a generic string
typedef unsigned long ulong;
using namespace std;
using std::cout;
using std::endl;

#endif

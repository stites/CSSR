////////////////////////////////////////////////////////////////////////
//Title:	Test.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for Test.cpp
//
////////////////////////////////////////////////////////////////////////

#ifndef TEST_H
#define TEST_H


#include "Common.h"

//For chi-square test
#define ITMAX 100
#define EPS 3.0e-7    //Relative accuracy
#define FPMIN 1.0e-30   //smallest floating point representation

//For KS-test
#define EPS1 0.001         //KS numbers
#define EPS2 1.0e-8        //KS numbers
#define SIGLEVEL 0.001    //Default chance of splitting when we shouldn't

enum testType {KS, CHIS};

class Test{
 private:
  testType m_type;
  //Chi-Square Functions
  double RunKSTest(double dist1[], int count1, double dist2[], int count2,
		   int distSize);
  double RunChiTest(double dist1[], int count1, double dist2[],int count2,
		    int distSize);
  float gammq(float a, float x);
  void gcf(float *gammcf, float a, float x, float *gln);
  void gser(float *gammser, float a, float x, float *gln);
  void nerror(char error_text[]);
  float gammln(float xx);
  void chstwo(double bins1[], int n1, double bins2[], int n2, int nbins,
	      int knstrn, float *df, float *chsq, double *prob);

  //KS Functions
  void KStwo(double data1[], unsigned long n1, double data2[],
	     unsigned long n2, double *d, double *prob, int dist_size);
  double ProbKS(double alam);

public:
  Test(bool isChi){if(isChi) m_type = CHIS; else m_type = KS;}
  ~Test();
  double RunTest(double dist1[], int count1, double dist2[], int count2,
		 int distSize);
};

#endif

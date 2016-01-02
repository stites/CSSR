///////////////////////////////////////////////////////////////////////////////
// Title       : Test
// Date        : July 23, 2003
// Author      : Kristina Klinkner
// Description : runs various statistical tests for use with CSSR
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
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

#include "Test.h"

//chstwo ---- calculates chi-square for two distributions
// After Numerical Recipes in C
void Test::chstwo(double bins1[], int n1, double bins2[], int n2, int nbins,
                  int knstrn, float *df, float *chsq, double *prob) {
  int j;
  float temp;

  *df = nbins - knstrn;
  *chsq = 0.0;
  float ratio1;
  float ratio2;

  if (n1 > 0) {
    ratio1 = (float) (((float) sqrt(n2)) / ((float) sqrt(n1)));
  }
  else {
    ratio1 = 0.0;
  }

  if (n2 > 0) {
    ratio2 = (float) (((float) sqrt(n1)) / ((float) sqrt(n2)));
  }
  else {
    ratio2 = 0.0;
  }

  for (j = 0; j < nbins; j++) {
    if (bins1[j] == 0.0 && bins2[j] == 0.0) {
      --(*df);                //No data means one less degree of freedom
    }
    else {
      temp = ratio1 * bins1[j] * n1 - ratio2 * bins2[j] * n2;
      *chsq += temp * temp / (bins1[j] * n1 + bins2[j] * n2);
    }
  }
  *prob = gammq(0.5 * (*df), 0.5 * (*chsq));
}


//gammq  ---- returns the incomplete gamma function Q(a,x) = 1 - P(a,x).
float Test::gammq(float a, float x) {
  float gamser, gammcf, gln;

  //cout <<"a " << a<< " x "<<x<<endl;
  if (x < 0.0 || a <= 0.0) {
    nerror("Invalid arguments in routine gammq");
  }
  if (x < (a + 1.0)) {      //use the series representation
    gser(&gamser, a, x, &gln);
    return 1.0 - gamser;                //and take its complement
  }
  else {
    gcf(&gammcf, a, x, &gln);  //use the continued fraction representation
    return gammcf;
  }
}


//gser --- Returns the incomplete gamma function P(a,x)
//evaluated by its series representation.  Also returns
//natural log of gamma(a)
void Test::gser(float *gamser, float a, float x, float *gln) {
  int n;
  float sum, del, ap;
  *gln = gammln(a);

  if (x <= 0.0) {
    if (x < 0.0) {
      nerror("x less than zero in series expansion gamma function");
    }
    *gamser = 0.0;
    return;
  }
  else {
    ap = a;
    del = sum = 1.0 / a;
    for (n = 1; n <= ITMAX; n++) {
      ++ap;
      del *= x / ap;
      sum += del;
      if (fabs(del) < (fabs(sum) * EPS)) {
        *gamser = sum * exp(-x + (a * log(x)) - (*gln));
        return;
      }
    }
    nerror("a is too large, ITMAX is too small, in series expansion gamma function");
    return;
  }
}

//gcf--- Returns the incomplete gamma function Q(a,x), evaulated by its
//continued fraction representation as gammcf.  Also returns natural log
//of gamma as gln
void Test::gcf(float *gammcf, float a, float x, float *gln) {
  int i;
  float an, b, c, d, del, h;

  *gln = gammln(a);
  b = x + 1.0 - a;
  c = 1.0 / FPMIN;
  d = 1.0 / b;
  h = d;

  for (i = 1; i <= ITMAX; i++) {     //iterate to convergence
    an = -i * (i - a);
    b += 2.0;      //Set up for evaluating continued
    d = an * d + b;  //fraction by modified Lentz's method with b_0 = 0.
    if (fabs(d) < FPMIN) {
      d = FPMIN;
    }
    c = b + an / c;
    if (fabs(c) < FPMIN) {
      c = FPMIN;
    }
    d = 1.0 / d;
    del = d * c;
    h *= del;

    if (fabs(del - 1.0) < EPS) {
      break;
    }
  }
  if (i > ITMAX) {
    nerror("a too large, ITMAX too small in continued fraction gamma function");
  }
  *gammcf = exp(-x + a * log(x) - (*gln)) * h;   //Put factors in front
  return;
}


//gammln --- returns natural log of gamma(xx), for xx> 0
float Test::gammln(float xx) {
  double x, y, tmp, ser;
  static double cof[6] = {76.18009172947146, -86.50532032941677,
                          24.01409824083091, -1.231739572450155,
                          0.1208650973866179e-2, -0.5395239384953e-5};
  int j;

  y = x = xx;
  tmp = x + 5.5;
  tmp -= (x + 0.5) * log(tmp);
  ser = 1.000000000190015;

  for (j = 0; j <= 5; j++) {
    ser += cof[j] / ++y;
  }
  return -tmp + log(2.5066282746310005 * ser / x);
}

void Test::nerror(const char error_text[]) {
  cerr << error_text << endl;
  exit(1);
}

double Test::RunTest(double dist1[], int count1, double dist2[], int count2, int distSize) {
  cout << "Running test --";
  cout << " Count1: " << std::to_string(count1);
  cout << " Count2: " << std::to_string(count2);
  cout << endl;
  if (m_type == KS) {
    return RunKSTest(dist1, count1, dist2, count2, distSize);
  }
  else if (m_type == CHIS) {
    return RunChiTest(dist1, count1, dist2, count2, distSize);
  }
  else {
    cerr << "No type of statistical test set" << endl;
    exit(1);
  }
}


double Test::RunKSTest(double dist1[], int count1, double dist2[], int count2, int distSize) {
  double KSstat;              //KS statistic
  double sigLevel;            //significance level for KS test

  KStwo(dist1, count1, dist2, count2, &KSstat, &sigLevel, distSize);
  return sigLevel;
}


double Test::RunChiTest(double dist1[], int count1, double dist2[], int count2, int distSize) {
  int constraints = 0;
  float chsq;
  double prob;
  float df = 1;

  chstwo(dist1, count1, dist2, count2, distSize, constraints, &df, &chsq, &prob);
  return prob;
}


///////////////////////////////////////////////////////////////////////////
// Function: Test::KStwo
// Purpose: calculates KS statistic and significance level (after
//          Numerical Recipies in C (pg 623))
// In Params: the size of the two distribution arrays to be tested,
//            the distribution arrays, the size of the alphabet
// Out Params: KS statistic and significance level
// In/Out Params: none
// Pre- Cond: distributions have been calculated and set into arrays
// Post-Cond: arrays are sorted and KS stat and sig level are known
//////////////////////////////////////////////////////////////////////////
void Test::KStwo(double data1[], unsigned long n1, double data2[],
                 unsigned long n2, double *d, double *prob, int dist_size) {
  unsigned long j = 0;
  double en1, en2, en, max;
  double *temp1 = new double[dist_size];
  double *temp2 = new double[dist_size];

  en1 = n1;
  en2 = n2;
  *d = 0.0;

  //obtain cumulative distributions

  temp1[0] = data1[0];
  temp2[0] = data2[0];

  for (int i = 1; i < dist_size; i++) {
    temp1[i] = data1[i] + temp1[i - 1];
    temp2[i] = data2[i] + temp2[i - 1];
  }

  //calculate KS statistic - take max difference between 2 values
  while (j < dist_size) {
    max = fabs(temp1[j] - temp2[j]);
    if (max > *d) {
      *d = max;
    }
    j++;
  }
  en = sqrt(en1 * en2 / (en1 + en2));

  //calculate significance level
  *prob = ProbKS((en + 0.12 + 0.11 / en) * (*d));

  delete[] temp1;
  delete[] temp2;
}


///////////////////////////////////////////////////////////////////////////
// Function: Test::ProbKS
// Purpose: calculates significance level (after Numerical Recipes in C)
// In Params: value needed to compute sig level from KS stat
// Out Params: significance level
// In/Out Params: none
// Pre- Cond: KS statistic has been calculated
// Post-Cond: sig level is  known
//////////////////////////////////////////////////////////////////////////
double Test::ProbKS(double alam) {
  int j;
  double a2, fac = 2.0, sum = 0.0, term, termbf = 0.0;

  a2 = -2.0 * alam * alam;

  for (j = 1; j <= 100; j++) {
    term = fac * exp(a2 * j * j);
    sum += term;
    if (fabs(term) <= EPS1 * termbf || fabs(term) <= EPS2 * sum) {
      return sum;
    }
    fac = -fac;
    termbf = fabs(term);
  }
  return 1.0;
}


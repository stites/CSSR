package CSSR.test

class Test {
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
      if (max > *d)
        *d = max;
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
      if (fabs(term) <= EPS1 * termbf || fabs(term) <= EPS2 * sum)
        return sum;
      fac = -fac;
      termbf = fabs(term);
    }
    return 1.0;
  }
}

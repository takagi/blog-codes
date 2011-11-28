#include <stdio.h>
#include <stdlib.h>

#define N 1000

int main ()
{
  double A[N][N], B[N][N], C[N][N];
  int i, j, k;
  
  // initialize the matrices
  for ( i = 0; i < N; i++ )
    for ( j = 0; j < N; j++ )
      {
        A[i][j] = 1.0;
        B[i][j] = 1.0;
        C[i][j] = 0.0;
      }
  
  // matrix multiplication
  int ibl = 100;
  int ib, jb, kb;
  for (ib=0; ib<N; ib+=ibl)
    for (jb=0; jb<N; jb+=ibl)
      for (kb=0; kb<N; kb+=ibl)
        for (i=ib; i<ib+ibl; i++)
          for (j=jb; j<jb+ibl; j++)
            for (k=kb; k<kb+ibl; k++)
              C[i][j] += A[i][k] * B[k][j];
  
  printf ( "%f\n", C[0][0] );
  
  return 0;
}

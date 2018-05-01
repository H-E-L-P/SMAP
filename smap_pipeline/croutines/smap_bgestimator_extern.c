#include <stdio.h>
#include <stdlib.h> //For qsort
#include <math.h> //For absdev
#include "idl_export.h"

//C version of bg estimator
//The basic premise it to take an image chunk and
// return the median value, iteratively removing
// 'sources' -- here defined as pixels more than
// N absolute deviatioins away from the median
// image   -- data to work with.  Must already be cleaned of NaNs, etc.
// working -- working array, same size as image
// nimage  -- number of elements in image
// nabsdev -- number of absolute deviations to reject pixels
// med     -- returns median
// nmed    -- number of elements used in median

/*
int smap_bgestimator_compare_doubles (const void *a, const void *b) {
  const double ia = * ((const double *)a); 
  const double ib = * ((const double *)b);
  double diff = ia - ib;
  if (diff == 0) return 0;
  if (ia < ib) return -1;
  return 1;
}

double smap_bgestimator_medval(double* array, IDL_LONG n) {
  //Sort em
  qsort(array, (size_t) n, sizeof(double), smap_bgestimator_compare_doubles );
  if (n % 2 == 0)
    return 0.5*(array[n/2]+array[n/2+1]);
  else
    return array[n/2];
}
*/

#define SWAP(a,b) temp=(a); (a)=(b); (b)=temp;

double smap_bgestimator_medval(double arr[],int n)
{
  int i,ir,j,l,mid,k;
  double a,temp;
  
  k = n/2; //Ignore the even length business

  l=0;
  ir=n-1;
  for (;;) {
    if (ir <= l+1) {
      if (ir == l+1 && arr[ir] < arr[l]) {
	SWAP(arr[l],arr[ir])
	  }
      return arr[k];
    } else {
      mid=(l+ir) >> 1;
      SWAP(arr[mid],arr[l+1])
	if (arr[l] > arr[ir]) {
	  SWAP(arr[l],arr[ir])
	    }
      if (arr[l+1] > arr[ir]) {
	SWAP(arr[l+1],arr[ir])
	  }
      if (arr[l] > arr[l+1]) {
	SWAP(arr[l],arr[l+1])
	  }
      i=l+1;
      j=ir;
      a=arr[l+1];
      for (;;) {
	do i++; while (arr[i] < a);
	do j--; while (arr[j] > a);
	if (j < i) break;
	SWAP(arr[i],arr[j])
	  }
      arr[l+1]=arr[j];
      arr[j]=a;
      if (j >= k) ir=j-1;
      if (j <= k) l=i;
    }
  }
}

#undef SWAP

int smap_bgestimator_natural( double *image, double* working,
			      IDL_LONG nimage, double *nabsdev, 
			      double *med, IDL_LONG* nmed ) {
  double medval, meanabsdev, testval;
  IDL_LONG i, j, maxit, ngood, nprevgood;

  maxit = 100; //Maximum number of iterations

  if (nimage < 3) return 1; //Failure
  if (*nabsdev <= 0.0) return 1; //Invalid
  
  //Copy into working and sort.  For the first
  // pass, we choose them all
  for (i = 0; i < nimage; ++i)
    working[i] = image[i];
  ngood = nimage;
  medval = smap_bgestimator_medval(working,ngood);

  //Now find absdev; it's okay that working has been rearranged
  meanabsdev = fabs(working[0]-medval);
  for (i = 1; i < ngood; ++i)
    meanabsdev += fabs(working[i]-medval);
  meanabsdev /= (double) ngood;

  //Main loop
  for (i = 0; i < maxit; ++i) {
    nprevgood = ngood;
    ngood = 0;
    //Copy accepted points to working
    //Test full image at every step
    testval = (*nabsdev)*meanabsdev;
    for (j=0; j < nimage; ++j) {
      if ( fabs(image[j] - medval) < testval )
	working[ngood++] = image[j];
    }
    if (ngood < 3) return 1; //Failure
    //If no new rejected points, return with value
    if (nprevgood == ngood) {
      *med = medval;
      *nmed = ngood;
      return 0;
    }
    //Get new values of medval and meanabsdev
    medval = smap_bgestimator_medval(working,ngood);
    meanabsdev = fabs(working[0]-medval);
    for (j = 1; j < ngood; ++j)
      meanabsdev += fabs(working[j]-medval);
    meanabsdev /= (double) ngood;

  }
  return 1; //Exceeded max iters
}

int smap_bgestimator_extern(int argc, void* argv[]) {
  if (argc != 6) return 1;
  return smap_bgestimator_natural((double*) argv[0], 
				  (double*) argv[1],
				  *( (IDL_LONG*) argv[2] ),
				  (double*) argv[3],
				  (double*) argv[4],
				  (IDL_LONG*) argv[5]);
}

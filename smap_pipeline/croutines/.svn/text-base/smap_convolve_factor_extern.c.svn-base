#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "idl_export.h" /* IDL external definitions */

//*******************************************
// Does convolution by a factorizable kernel
// with wrapping, NaN/inf protection, and normalization
// by 1/ SUM(abs(kernel2d))

//n1, n2  Size of input image
//image   Image, in column major order.  Changed on output
//kn      Number of elements in kernel factor
//kernel  Kernel factor

//The kernel is the outer product of kernel with itself -- that is,
// this particular implementation goes further and requires a symmetric
// kernel
//Return 0 on success, 1 on failure

//Base version with no wrapping, no checks for finitness, and
//  no normalization
int smap_convolve_factor_base_natural( IDL_LONG n1, IDL_LONG n2, 
				       double* image, IDL_LONG kn, 
				       double* kernel ) {
  //Input test
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0; //Since the cluster compiler doesn't support NAN
  double currval;
  double *colptr, *subarrptr, *work;
  kno2 = kn/2;

  //The idea here is to take advantage of the fact that
  // if the kernel can be factored into two one dimensional
  // arrays, then the convolution can similarly be factored.
  // And doing two 1D convolutions is much faster than the 2D case
  
  //We will need a working array the same size as the input array
  // since there are two convolutions
  work = (double*) malloc( sizeof(double)*n1*n2 );
  
  //There is one tricky index game here.  The input image (from IDL)
  // is in column-major order (like fortran).  We have two convolutions
  // to do, once along each access.  To improve the way this accesses 
  // memory, it is helpful to do the first convolution into work stored
  // in row-major order, then the second convolution back from that
  // to the input array in column-major order.  Tricksy!

  //So, this one is the first convolution which is along the first
  // index of the array
  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;

    for (idx1 = 0; idx1 < kno2; ++idx1) {
      minidx = kno2 - idx1; 
      subarrptr = colptr + idx1 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];

      //Store currval in row-major order
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      //This is the main body (no edge effects)
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < kn; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      work[idx1*n2+idx2]      = currval;
    }

    //Top part, has edge effects
    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1); //Limit on top end of array
      subarrptr = colptr + idx1 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      work[idx1*n2+idx2]      = currval;
    }
  }

  //Now, convolve along the second index, store that back in image
  // in column-major order.  This is pretty much exactly the same
  // thing all over again, but with some indices flipped and the role
  // of work/image flipped
  double *workptr;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    workptr = work + idx1*n2;

    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;
      currval = 0;
      subarrptr = workptr + idx2 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      subarrptr  = workptr + idx2 - kno2;
      currval = 0;
      for (idx3 = 0; idx3 < kn; ++idx3)
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      subarrptr  = workptr + idx2 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
  }

  //Clean up
  free(work);
  return 0;
}

//Version with no wrapping, no normalization, but checking
// for finiteness
int smap_convolve_factor_finite_natural( IDL_LONG n1, IDL_LONG n2, 
					 double* image, IDL_LONG kn, 
					 double* kernel ) {
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0; //Since the cluster compiler doesn't support NAN
  double currval, currimval;
  double *colptr, *subarrptr, *work;
  kno2 = kn/2;
  work = (double*) malloc( sizeof(double)*n1*n2 );
  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;

    for (idx1 = 0; idx1 < kno2; ++idx1) {
      minidx = kno2 - idx1; 
      subarrptr = colptr + idx1 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2+idx2]      = currval;
    }

    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1);
      subarrptr = colptr + idx1 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2+idx2]      = currval;
    }
  }

  double *workptr;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    workptr = work + idx1*n2;
    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;
      currval = 0;
      subarrptr = workptr + idx2 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      subarrptr  = workptr + idx2 - kno2;
      currval = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      subarrptr  = workptr + idx2 - kno2;
      currval = 0.0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
  }
  free(work);
  return 0;
}

//This version has wrapping
int smap_convolve_factor_wrap_natural( IDL_LONG n1, IDL_LONG n2, 
				       double* image, IDL_LONG kn, 
				       double* kernel ) {
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0; //Since the cluster compiler doesn't support NAN
  double *colptr, *subarrptr, currval, *work;
  kno2 = kn/2;
  double currimval;

  work = (double*) malloc( sizeof(double)*n1*n2 );

  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;

    //There are two types of step here -- the wrapped part (which happens
    // at the top and bottom) and the main, unwrapped bit    
    for (idx1 = 0; idx1 < kno2; ++idx1) {
      //This is the bottom wrap
      minidx = kno2 - idx1; //Minimum index of kernel used in unwrapped part
      
      //wrapped part
      subarrptr = colptr + idx1 + n1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < minidx; ++idx3)
	currval += subarrptr[idx3]*kernel[idx3];
      //unwrapped part
      subarrptr = colptr + idx1 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      //This is the main body (no wrapping)
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < kn; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      work[idx1*n2+idx2] = currval;
    }

    //Top part, has wrapping
    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1); //Limit on top end of array
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      
      subarrptr = colptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3)
	currval += subarrptr[idx3]*kernel[idx3];
      work[idx1*n2+idx2] = currval;
    }
  }


  double *workptr;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    workptr = work + idx1*n2;

    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;
      subarrptr = workptr + idx2 + n2 - kno2;

      currval = 0;
      for (idx3 = 0; idx3 < minidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      
      subarrptr = workptr + idx2 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      subarrptr  = workptr + idx2 - kno2;

      currval = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      subarrptr  = workptr + idx2 - kno2;
      currval = 0; 
      for (idx3 = minidx; idx3 < maxidx; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];

      subarrptr = workptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3) 
	currval += subarrptr[idx3]*kernel[idx3];
      image[idx1 + idx2*n1] = currval;
    }
  }

  //Clean up
  free(work);

  return 0;
}

//This version has wrapping and NaN checking
int smap_convolve_factor_fw_natural( IDL_LONG n1, IDL_LONG n2, 
				     double* image, IDL_LONG kn, 
				     double* kernel ) {
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0; 
  double *colptr, *subarrptr, *work;
  kno2 = kn/2;
  double currval, currimval;

  work = (double*) malloc( sizeof(double)*n1*n2 );

  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;
    for (idx1 = 0; idx1 < kno2; ++idx1) {
      minidx = kno2 - idx1;
      subarrptr = colptr + idx1 + n1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < minidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      //unwrapped part
      subarrptr = colptr + idx1 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      //This is the main body (no wrapping)
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2+idx2] = currval;
    }

    //Top part, has wrapping
    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1); //Limit on top end of array
      subarrptr = colptr + idx1 - kno2;
      currval = 0; 
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      
      subarrptr = colptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      work[idx1*n2+idx2] = currval;
    }
  }
  double *workptr;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    workptr = work + idx1*n2;

    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;
      subarrptr = workptr + idx2 + n2 - kno2;

      currval = 0;
      for (idx3 = 0; idx3 < minidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      
      subarrptr = workptr + idx2 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      subarrptr  = workptr + idx2 - kno2;

      currval = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      subarrptr  = workptr + idx2 - kno2;
      currval = 0; 
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }

      subarrptr = workptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval))
	  currval += currimval*kernel[idx3];
      }
      image[idx1 + idx2*n1] = currval;
    }
  }

  //Clean up
  free(work);

  return 0;
}

//This version has finiteness checking, normalization, but not wrapping

int smap_convolve_factor_fn_natural( IDL_LONG n1, IDL_LONG n2, 
				     double* image, IDL_LONG kn, 
				     double* kernel ) {
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0;
  double *colptr, *subarrptr, *kernfac, *kerabs, *work;
  kno2 = kn/2;
  double currval, currksum, currimval;

  // We also need to deal with the normalization while allowing for
  // missing data, which takes some more working arrays
  work = (double*) malloc( sizeof(double)*n1*n2 );
  kernfac = (double*) malloc( sizeof(double)*n1*n2 );
  kerabs = (double*) malloc( sizeof(double)*kn );
  
  //The normalization uses the absolute value of the kernel
  for (idx1 = 0; idx1 < kn; ++idx1)
    kerabs[idx1] = fabs(kernel[idx1]);

  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;
    for (idx1 = 0; idx1 < kno2; ++idx1) {
      minidx = kno2 - idx1; 
      
      //Edge effect part
      subarrptr = colptr + idx1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	} 
      }
      //Store currval and kernfac in row-major order
      kernfac[idx1*n2+idx2]   = currksum;
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      //This is the main body (no edge effects)
      subarrptr = colptr + idx1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }

      kernfac[idx1*n2 + idx2] = currksum;
      work[idx1*n2+idx2]      = currval;
    }

    //Top part, has edge effects
    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1); //Limit on top end of array
      subarrptr = colptr + idx1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval  += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }
      kernfac[idx1*n2+idx2] = currksum;
      work[idx1*n2+idx2]    = currval;
    }
  }

  /*
  for (idx1 = 0; idx1 < n1; ++idx1)
    for (idx2 = 0; idx2 < n2; ++idx2)
      image[idx1+idx2*n1] = work[idx1*n2+idx2] / kernfac[idx1*n2+idx2];
  return 0;
  */

  double *workptr,*kernfacptr,*kernsubptr;
  IDL_LONG offset;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    offset = idx1*n2;
    workptr = work + offset;
    kernfacptr = kernfac + offset;

    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;

      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr = workptr + offset;

      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr  = workptr + offset;
      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr  = workptr + offset;
      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
  }

  //Clean up
  free(work);
  free(kernfac);
  free(kerabs);

  return 0;
}


//This version has finiteness checking, normalization, and wrapping

int smap_convolve_factor_fwn_natural( IDL_LONG n1, IDL_LONG n2, 
						   double* image, 
						   IDL_LONG kn, 
						   double* kernel ) {
  //Input test
  if (n1 == 0) return 1;
  if (n2 == 0) return 1;
  if (kn == 0) return 1;
  
  IDL_LONG idx1, idx2, idx3, minidx, maxidx, kno2;
  double nanval = 0.0/0.0; //Since the cluster compiler doesn't support NAN
  double *colptr, *subarrptr, currval, *kernfac, *kerabs, *work;
  kno2 = kn/2;

  //The idea here is to take advantage of the fact that
  // if the kernel can be factored into two one dimensional
  // arrays, then the convolution can similarly be factored.
  // And doing two 1D convolutions is much faster than the 2D case
  
  double currksum, currimval;

  //We will need a working array the same size as the input array
  // since there are two convolutions.  We also need to deal
  // with the normalization and missing data, which takes some more 
  // working arrays
  work = (double*) malloc( sizeof(double)*n1*n2 );
  kernfac = (double*) malloc( sizeof(double)*n1*n2 );
  kerabs = (double*) malloc( sizeof(double)*kn );
  
  //The normalization uses the absolute value of the kernel
  for (idx1 = 0; idx1 < kn; ++idx1)
    kerabs[idx1] = fabs(kernel[idx1]);

  //There is one tricky index game here.  The input image (from IDL)
  // is in column-major order (like fortran).  We have two convolutions
  // to do, once along each access.  To improve the way this accesses 
  // memory, it is helpful to do the first convolution into work stored
  // in row-major order, then the second convolution back from that
  // to the input array in column-major order.  Tricksy!

  //So, this one is the first convolution which is along the first
  // index of the array
  for (idx2 = 0; idx2 < n2; ++idx2) {
    colptr = image + idx2*n1;
    maxidx = kn;

    //There are two types of step here -- the wrapped part (which happens
    // at the top and bottom) and the main, unwrapped bit    
    for (idx1 = 0; idx1 < kno2; ++idx1) {
      //This is the bottom wrap
      minidx = kno2 - idx1; //Minimum index of kernel used in unwrapped part
      
      //wrapped part
      subarrptr = colptr + idx1 + n1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < minidx; ++idx3) {
	currimval = subarrptr[idx3];
	if ( finite( currimval ) ) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }
      //unwrapped part
      subarrptr = colptr + idx1 - kno2;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	} 
      }

      //Store currval and kernfac in row-major order
      kernfac[idx1*n2 + idx2] = currksum;
      work[idx1*n2 + idx2]    = currval;
    }

    for (idx1 = kno2; idx1 < n1 - kno2; ++idx1) {
      //This is the main body (no wrapping)
      subarrptr = colptr + idx1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }

      kernfac[idx1*n2 + idx2] = currksum;
      work[idx1*n2+idx2]      = currval;
    }

    //Top part, has wrapping
    minidx = 0;
    for (idx1 = n1-kno2; idx1 < n1; ++idx1) {
      maxidx = kno2 + (n1-idx1); //Limit on top end of array

      subarrptr = colptr + idx1 - kno2;
      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval  += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }
      
      subarrptr = colptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3];
	}
      }

      kernfac[idx1*n2 + idx2] = currksum;
      work[idx1*n2+idx2]      = currval;
    }
  }

  /*
  for (idx1 = 0; idx1 < n1; ++idx1) {
    for (idx2 = 0; idx2 < n2; ++idx2)
      printf(" %f",kernfac[idx1*n2+idx2]);
    printf("\n");
  }
    
  for (idx1 = 0; idx1 < n1; ++idx1)
    for (idx2 = 0; idx2 < n2; ++idx2)
      image[idx1+idx2*n1] = work[idx1*n2+idx2] / kernfac[idx1*n2+idx2];
  return 0;
  */

  //Now, convolve along the second index, store that back in image
  // in column-major order.  This is pretty much exactly the same
  // thing all over again, but with some indices flipped and the role
  // of work/image flipped
  double *workptr,*kernfacptr,*kernsubptr;
  IDL_LONG offset;
  for (idx1 = 0; idx1 < n1; ++idx1) {
    maxidx = kn;
    workptr = work + idx1*n2;
    kernfacptr = kernfac + idx1*n2;

    for (idx2 = 0; idx2 < kno2; ++idx2) {
      minidx = kno2 - idx2;

      offset = idx2 + n2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr = workptr + offset;

      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < minidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	} 
      }
      
      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr = workptr + offset;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
    for (idx2 = kno2; idx2 < n2-kno2; ++idx2) {
      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr  = workptr + offset;

      currval = 0; currksum = 0;
      for (idx3 = 0; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
    minidx = 0;
    for (idx2 = n2-kno2; idx2 < n2; ++idx2) {
      maxidx = kno2 + (n2-idx2);
      offset = idx2 - kno2;
      kernsubptr = kernfacptr + offset;
      subarrptr  = workptr + offset;
      currval = 0; currksum = 0;
      for (idx3 = minidx; idx3 < maxidx; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }

      kernsubptr = kernfacptr - maxidx;
      subarrptr = workptr - maxidx;
      for (idx3 = maxidx; idx3 < kn; ++idx3) {
	currimval = subarrptr[idx3];
	if (finite(currimval)) {
	  currval += currimval*kernel[idx3];
	  currksum += kerabs[idx3]*kernsubptr[idx3];
	}
      }
      if (currksum > 0) currval /= currksum; else currval=nanval;
      image[idx1 + idx2*n1] = currval;
    }
  }

  //Clean up
  free(work);
  free(kernfac);
  free(kerabs);

  return 0;
}

/*
  Arguments:
  n1         -- image size along dimension 1
  n2         -- image size along dimension 2
  image      -- image data
  kn         -- number of elements in kernel
  kernel     -- kernel data
  normalize  -- flag saying whether to normalize by the ABS of the 2d kernel
  nancheck   -- check for NaN in image pixels
  wrap       -- wrap convolution around edges.  Otherwise, zero pad

 */
int smap_convolve_factor_extern(int argc, void* argv[]) {
  if (argc != 8) return 0;
  IDL_LONG normalize = *((IDL_LONG*) argv[5]);
  IDL_LONG nancheck = *((IDL_LONG*) argv[6]);
  IDL_LONG wrap = *((IDL_LONG*) argv[7]);
  if (normalize) {
    if (nancheck) {
      if (wrap) 
	return smap_convolve_factor_fwn_natural( *((IDL_LONG*) argv[0]),
						 *((IDL_LONG*) argv[1]),
						 (double*) argv[2],
						 *((IDL_LONG*) argv[3]),
						 (double*) argv[4] );
      else
	return smap_convolve_factor_fn_natural( *((IDL_LONG*) argv[0]),
						*((IDL_LONG*) argv[1]),
						(double*) argv[2],
						*((IDL_LONG*) argv[3]),
						(double*) argv[4] );
    } else return 1; //Not implemented
  } else {
    if (nancheck) {
      if (wrap)
	return smap_convolve_factor_fw_natural( *((IDL_LONG*) argv[0]),
						*((IDL_LONG*) argv[1]),
						(double*) argv[2],
						*((IDL_LONG*) argv[3]),
						(double*) argv[4] );
      else 
	return smap_convolve_factor_finite_natural( *((IDL_LONG*) argv[0]),
						    *((IDL_LONG*) argv[1]),
						    (double*) argv[2],
						    *((IDL_LONG*) argv[3]),
						    (double*) argv[4] );
    } else {
      if (wrap) 
	return smap_convolve_factor_wrap_natural( *((IDL_LONG*) argv[0]),
						  *((IDL_LONG*) argv[1]),
						  (double*) argv[2],
						  *((IDL_LONG*) argv[3]),
						  (double*) argv[4] );
      else return smap_convolve_factor_base_natural( *((IDL_LONG*) argv[0]),
						     *((IDL_LONG*) argv[1]),
						     (double*) argv[2],
						     *((IDL_LONG*) argv[3]),
						     (double*) argv[4] );
    }
  }
}

#include <stdio.h>  
#include "idl_export.h"         /* IDL external definitions */  

//*************************************************************************
// module for binning signal into map
// Nov. 17, 2009
// Gaelen Marsden (gmarsden@phas.ubc.ca)
//
// Architecture borrowed from "simple_vars.c" and "example.c" in IDL
// examples directory. See 
// http://idlastro.gsfc.nasa.gov/idl_html_help/Basic_C_Examples.html
//
// Example call from IDL:
//
//   status = CALL_EXTERNAL(SMAP_GET_CALLEXT(), $
//			    'smap_accumulatemap_extern', $  
//			    DOUBLE(signal), DOUBLE(invvar), $
//                          LONG(pixind), LONG(nkeep), $
//			    DOUBLE(signalmap), DOUBLE(errormap), $
//			    DOUBLE(weightmap), DOUBLE(exposuremap), $
//                          DOUBLE(samptime), LONG(npix), LONG(doweight),$
//                          VALUE=[0,1,0,1,0,0,0,0,1,1,1], $
//			    /CDECL)
//
// Note that parameters 2, 4, 9 and 10, 11 (0-based) are pass-by-value.
//
// Shared object file is created by MAKE_DLL. See "smap_get_callext.pro".
//
// HISTORY:
// 2009-11-17 (gm): initial version
// 2011-08-24 (ac): exposure now double, add samptime argument, weight -> invvar,
//                  no longer an array
//
//*************************************************************************

int smap_accumulatemap_extern_natural(double *signal, double *var,
				      IDL_LONG *pixind, IDL_LONG ndata,
				      double *image, double *error,
				      double *weightmap, double *exposure, 
				      double *samptime, IDL_LONG npix, 
				      IDL_LONG dovarweight)
{
  // loop over data samples and drop into pixels
  // maps are stored in column-major format (IDL's convention), ie
  // first index moves the quickest

  // dovarwght controls whether inverse variance weighting is used (if set), or if
  // samptime weighting is used (if not set).  The latter is necessary so we can
  // combine parallel and non-parallel scans (which have different sampling
  // rates)

  IDL_LONG i, ind;

  //For some reason when I try to pass in var/sampletime by value
  // (instead of as pointers), I get seg faults.  Why does this work
  // for IDL_LONG, but not double?  Very strange.
  double sampletime = *samptime;
  double variance   = *var;

  if (dovarweight == 1) {
    //Variance weighting
    double weightval = 1.0/variance;
    for (i=0; i<ndata; i++) {
      ind = pixind[i];
      // make sure pixel is in bounds
      if ( ind >= 0 && ind < npix ) {
	image[ind] += weightval * signal[i];
	error[ind] += weightval; //variance * weight^2 = 1/variance = weight
	weightmap[ind] += weightval;
	exposure[ind] += sampletime;
      }
    }

  } else {
    //The default is to weight by sample time if not explicitly
    // given a weight
    double weightvalsq = sampletime*sampletime;
    for (i=0; i<ndata; i++) {
      ind = pixind[i];
      // make sure pixel is in bounds
      if ( ind >= 0 && ind < npix ) {
	image[ind] += sampletime * signal[i];
	error[ind] += variance * weightvalsq;
	weightmap[ind] += sampletime;
	exposure[ind] += sampletime;
      }
    }

  }

  return 1;
}


int smap_accumulatemap_extern(int argc, void* argv[])
{
  /* Ensure that the correct number of arguments were passed in */  
  if(argc != 11) return 0;  

  return smap_accumulatemap_extern_natural((double *)     argv[0], 
					   (double *)     argv[1],
					   (IDL_LONG *)   argv[2],
					   (IDL_LONG)     argv[3],
					   (double *)     argv[4],
					   (double *)     argv[5],
					   (double *)     argv[6],
					   (double *)     argv[7],
					   (double *)     argv[8],
					   (IDL_LONG)     argv[9],
					   (IDL_LONG)     argv[10]);
}



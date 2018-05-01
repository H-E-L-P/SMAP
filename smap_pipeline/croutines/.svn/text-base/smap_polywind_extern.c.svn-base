#include <stdio.h>  
#include "idl_export.h"         /* IDL external definitions */  

//C version of polywind (2D)
// nvertex -- number of vertices 
// xvertex -- X vertex positions
// yvertex -- Y vertex positions
// npoints -- number of points to evaluate
// xvals   -- x positions
// yvals   -- y positions
// wind    -- returns winding number
int smap_polywind_natural(IDL_LONG nvertex, double* xvertex,
			  double* yvertex, IDL_LONG npoints,
			  double* xvals, double* yvals,
			  IDL_LONG* wind) {
  IDL_LONG idx1,idx2,windval;
  double cx, cy, currxvert, curryvert, prevxvert, prevyvert;
  double cprod;

  for (idx1 = 0; idx1 < npoints; ++idx1) {
    cx = xvals[idx1]; cy = yvals[idx1];
    windval = 0;
    prevxvert = xvertex[nvertex-1];
    prevyvert = yvertex[nvertex-1];
    for (idx2 = 0; idx2 < nvertex; ++idx2) {
      currxvert = xvertex[idx2];
      curryvert = yvertex[idx2];
      cprod = (prevxvert-cx)*(curryvert-cy)-(prevyvert-cy)*(currxvert-cx);
      
      if (prevyvert <= cy) {
	if ((curryvert > cy) && (cprod > 0)) ++windval;
      } else {
	if ((curryvert <= cy) && (cprod < 0)) --windval;
      }
      prevxvert = currxvert;
      prevyvert = curryvert;
    }
    wind[idx1] = windval;
  }

  return 0;
}

int smap_polywind_extern(int argc, void* argv[]) {
  if (argc != 7) return 0;
  return smap_polywind_natural( *((IDL_LONG*) argv[0]),
				(double*) argv[1],
				(double*) argv[2],
				*((IDL_LONG*) argv[3]),
				(double*) argv[4],
				(double*) argv[5],
				(IDL_LONG*) argv[6]);
}

SMAP details
============

HerMES maps are created by the SMAP pipeline. The SMAP map-maker iteratively
removes a low-order polynomial baseline from each scan. At each iteration
:math:`i` a polynomial is fit to the time-stream residual :math:`R_i = S - M_{i-1}`, where :math:`S` is
the time stream and :math:`M_{i-1}` is the predicted time stream given the map
calculated on the previous iteration. Additionally, each scan is given a weight
based on the inverse variance of the time-stream residual. The order or the
polynomial baseline varies from 0 to 9, depending on the scan length, with
longer scans requiring higher order polynomials. The polynomial order is chosen
using an automated algorithm which is only a function of the scan length. These
maps are made with 20 iterations, which appears to provide sufficient
convergence. The mapping algorithm is described in [3] and [4].

Standard maps
-------------
Each fits file in this data release contains 4 extensions:

  1.  signal map [Jy / beam]
  2.  error map, based on propagation of time-stream weights [Jy / beam]
  3.  exposure map [sec]
  4.  mask map with the following values:
    -  0: no mask (ie good data)
    -  1: regions with low depth relative to the rest of the map

Complementary maps
------------------
As well as the standard map output files, SMAP can produce complementary maps, which may be useful for certain projects
(e.g. map based statistics):

-   ang1:   sub-map made from all scans in one orientation on sky (eg horizontal)
-   ang2:   sub-map made from all scans in other orientation on sky (eg vertical)
-   bolo1:  sub-map half of detectors in focal plane
-   bolo2:  sub-map other half of detectors in focal plane
-   half1:  sub-map made from first half (in time) of data
-   half2:  sub-map made from second half (in time) of data
-   subfield: e.g., UDS for XMM-LSS-NEST.  Only contains data for a particular AOR set.

In some fields, there is more than one set of rotation angles. Here, we provide,
e.g., ang1a/ang2a and ang1b/ang2b jack-knife maps where possible.

The complementary maps are not iterated, but simply use the offset and weights
solution calculated for the full "image" map, as well as the cosmic ray
information.

Additional Notes
-----------------

Changes to the SMAP algorithm as described in [3] and [4] compared with the
DR1 release:

* The algorithm for the temperature-correlation removal has been modified
  slightly to use more robust fitting.

* One must use a full implementation of the WCS specification to use the
  astrometry for these (and all previous) SMAP maps. In particular, for far
  northern or southern fields, LATPOLE and LONPOLE do not have their standard
  values to reduce field distortion. Currently, this only affects the GOODS-N
  and MS1358 fields.

References
----------

[1] Oliver, S.~J., Bock, J., et al. 2012, MNRAS, 424, 1614

[2] Chapin, E.L., et al. 2011, MNRAS, 411, 505

[3] Levenson, L., Marsden, G., Zemcov, M., et al. 2010, MNRAS, 409, 83

[4] Viero, M., et al. 2013, ApJ, 772, 77

[5] Viero, M., et al. 2014, ApJS, 210, 22

.. toctree::
   :maxdepth: 2
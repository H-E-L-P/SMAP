import math
import numpy
import scipy.optimize

"""Modified greybody SED routine"""

def alpha_merge_eqn(x,alpha,beta,x0) :
    """Equation we need the root for to merge power law to modified
    blackbody"""
    try :
        #This can overflow badly
        bterm = xox0beta / math.expm1((x / x0)**beta)
    except OverflowError:
        #If xox0beta is very large, then the bterm is zero
        bterm = 0.0
    return x - (1-math.exp(-x))*(3+alpha+beta*bterm)

def greybody(wave,T,beta,lambda0,alpha,fnorm,wavenorm=500.0,noalpha=False) :
    """Modified blackbody calculator
    
    Inputs:
    wave:     Wavelength that flux densities are desired at, in microns
    T:        Temperature in Kelvin
    beta:     Dust slope parameter
    lambda0:  Wavelength where optical depth is unity, in microns
    alpha:    Power law slope on blue side of SED
    fnorm:    Normalizing flux at wavenorm

    Optional Inputs:
    wavenorm  Wavelength of normalization flux, in microns (def: 500)
    
    Keywords:
    noalpha   If set, don't use blue side power law
    """

    #Some constants
    c = 299792458e6 #in microns
    h = 6.6260693e-34 #J/s
    k = 1.3806505e-23 #J/K
    hcokt = h * c / (k*T)

    # Convert wavelengths to x = h nu / k T
    x = hcokt / numpy.array(wave)
    x0 = hcokt / lambda0
    xnorm = hcokt / wavenorm

    #Two cases -- with power law merge and without
    if noalpha :
        #simple version
        #Figure out normalization
        normfac = -fnorm * math.expm1(xnorm) / \
            (math.expm1(-(xnorm / x0)**beta) * xnorm**3)
        retval = -normfac * numpy.expm1(-(x / x0)**beta) * x**3 / \
            numpy.expm1(x)
    else :
        #complicated version -- merges onto power law
        #The first step is to figure out the x (frequency)
        # where the join happens.  All frequencies above this
        # (so x > xmerge) are on the power law (blue side)
        a    = 0.01
        b    = 30.0
        aval = alpha_merge_eqn(a,alpha,beta,x0)
        bval = alpha_merge_eqn(b,alpha,beta,x0)
        if (aval*bval > 0) :  #Should have opposite signs!
            errmsg="Couldn't find alpha merge point for T: "\
                "%f beta: %f lambda0: %f alpha %f, f(%f): %f f(%f): %f"
            errmsg %= (T,beta,lambda0,alpha,a,aval,b,bval)
            raise ValueError(errmsg)
        xmerge = scipy.optimize.brentq(alpha_merge_eqn,a,b,
                                       args=(alpha,beta,x0),
                                       disp=True)
        #Get merge constant -- note this is -before- flux normalization
        # to allow for the case where wavenorm is on the power law part
        kappa = -xmerge**(3+alpha) * math.expm1(-(xmerge/x0)**beta) / \
            math.expm1(xmerge)

        #Do basic computation
        retval = numpy.empty( len(wave) )
        ispower = x > xmerge
        retval[ispower] = kappa * (x[ispower]**(-alpha))
        retval[~ispower] = - numpy.expm1(-(x[~ispower]/x0)**beta) * \
            x[~ispower]**3/numpy.expm1(x[~ispower])
    
        #Compute normalization constant and apply
        if xnorm > xmerge :
            normfac = fnorm * xnorm**alpha / kappa
        else :
            normfac = -fnorm * math.expm1(xnorm) / \
                (xnorm**3 * math.expm1(-(xnorm/x0)**beta))
        retval *= normfac

    return retval

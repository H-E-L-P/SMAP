#!/usr/bin/env python

"""MCMC model for modified greybody fit to far-IR/sub-mm/mm photometry.
This works in the observer frame."""

#Requires that numpy, scipy, emcee, asciitable, pyfits are installed

import numpy
import emcee
import math
import greybody


"""Class holding data, defining likelihood"""
class ModifiedBlackbody(object) :
    def __init__(self, photfile, covfile=None, covextn=0, 
                 wavenorm=500.0, redshift=None, noalpha=False) :
        """Photfile is the name of the photometry file, covfile the name
        of a fits file holding the covariance matrix (if present), which
        is in extension covextn.  The wavelength normalizing the SED is
        wavenorm (in microns)."""
        self.read_phot( photfile )
        self.z = redshift
        self.has_covmatrix = False
        self.wavenorm = wavenorm
        self.noalpha = noalpha
        if not covfile is None :
            self.read_cov( covfile, extn=covextn )
        #set which params are fixed
        self.fixed = { 'T': False, 'beta': False, 'alpha' : False,
                       'lambda0' : False, 'fnorm' : False }
        #setup parameter limits
        #All have lower limits, but maybe not upper ones
        self.lowlim = { 'T': 0.0, 'beta': 0.0, 'alpha' : 0.0,
                        'lambda0' : 0.0, 'fnorm' : 0.0 }
        self.uplim = {}
        self.neginf = float("-inf")

    def read_phot( self, filename ) :
        """Reads in the photometry file, storing the wave [um],
        flux [mJy] and uncertainties [mJy]"""
        import asciitable
        data = asciitable.read(filename,comment='^#')
        if len(data) == 0 :
            errstr = "No data read from %s" % filename
            raise IOError(errstr)
        self.wave     = numpy.array([dat[0] for dat in data])
        self.flux     = numpy.array([dat[1] for dat in data])
        self.flux_unc = numpy.array([dat[2] for dat in data])
        self.ivar     = 1.0/self.flux_unc**2

    def read_cov( self, filename, extn=0 ) :
        """Reads in the covariance matrix from the specified
        extension of the input FITS file (in extension extn)"""
        import pyfits
        import numpy.linalg
        hdu = pyfits.open(filename)
        self.covmatrix = hdu[extn].data
        if self.covmatrix.shape[0] != self.covmatrix.shape[1] :
            errstr = "Covariance matrix from %s is not square" % filename
            raise ValueError(errstr)
        if self.covmatrix.shape[0] != len(self.flux) :
            errstr = "Covariance matrix doesn't have same number of datapoints as photometry"
            raise ValueError(errstr)
        self.invcovmatrix = numpy.linalg.inv(self.covmatrix)
        self.has_covmatrix = True

    def fix_param(self, param):
        """Fixes the specified parameter.

        Valid params are 'T','beta','alpha','lambda0','fnorm'
        """
        self.fixed[param] = True

    def unfix_param(self, param):
        """Un-fixes the specified parameter.

        Valid params are 'T','beta','alpha','lambda0','fnorm'
        """
        self.fixed[param] = False

    def set_lowlim(self, param, val) :
        """Sets the specified parameter lower limit to value.
        Valid params are 'T','beta','alpha','lambda0','fnorm'"""
        self.lowlim[param] = val

    def set_uplim(self, param, val) :
        """Sets the specified parameter upper limit to value.
        Valid params are 'T','beta','alpha','lambda0','fnorm'"""
        self.uplim[param] = val

    def checklim(self,pars) :
        """Checks to see if a given parameter set passes the limits.
        Returns True if it passes, False if it doesn't"""
        #I do have to wonder if it's worth finding a way to make
        # this more efficient, since it does get called a lot
        #First check lower, where we know they all exist
        if not self.fixed['T'] and pars[0] <= self.lowlim['T'] : return False
        if not self.fixed['beta'] and pars[1] <= self.lowlim['beta'] : 
            return False
        if not self.noalpha:
            if not self.fixed['alpha'] and pars[2] <= self.lowlim['alpha']: 
                return False
        if not self.fixed['lambda0'] and pars[3] <= self.lowlim['lambda0']:
            return False
        if not self.fixed['fnorm'] and pars[4] <= self.lowlim['fnorm']: 
            return False
        if len(self.uplim) == 0 : return True #quick exit
        if self.uplim.has_key('T') and not self.fixed['T']:
            if pars[0] > self.uplim['T'] : return False
        if self.uplim.has_key('beta') and not self.fixed['beta']:
            if pars[1] > self.uplim['beta'] : return False
        if not self.noalpha and self.uplim.has_key('alpha') and\
                not self.fixed['alpha']:
            if pars[2] > self.uplim['alpha'] : return False
        if self.uplim.has_key('lambda0') and not self.fixed['lambda0']:
            if pars[3] > self.uplim['lambda0'] : return False
        if self.uplim.has_key('fnorm') and not self.fixed['fnorm']:
            if pars[4] > self.uplim['fnorm'] : return False
        return True

    def getsed(self,pars,waveln) :
        """Returns the model SED at the specified wavelength -- which
        can be an array (numpy or otherwise)

        The order of pars is T, beta, alpha, lambda0, fnorm
        """
        return greybody.greybody(numpy.array(waveln),pars[0],pars[1],pars[3],
                                 pars[2],pars[4],wavenorm=self.wavenorm,
                                 noalpha=self.noalpha)

    def __call__(self,pars) :
        """Gets log likelihood assuming Gaussian errors and flat priors:
        log P( pars | data )"""
        #Since these are unbounded from above, should I use log priors?
        if not self.checklim(pars) : return self.neginf
        diff = self.flux - self.getsed(pars,self.wave)
        if self.has_covmatrix :
            return -0.5*numpy.dot(diff,numpy.dot(self.invcovmatrix,diff))
        else :
            return -0.5*numpy.sum(diff**2*self.ivar)

    def freqint(self, chain, minfreq, maxfreq, step=0.1) :
        """Integrates greybody between minfreq/maxfreq (in GHz)
        in erg/s/cm^2 (assuming fnorm is mJy), taking an array of parameters
        in the order T, beta, alpha, lambda0, fnorm"""
        nchain = chain.shape[0]
        integral = numpy.zeros(nchain)
        freq = numpy.arange(minfreq,maxfreq,step)
        waveln = 299792458e-3/freq
        ifac = step*1e-17 
        for i in xrange(nchain) :
            pars = chain[i,:]
            model = self.getsed(pars,waveln)
            integral[i] = numpy.sum(model)*ifac
        return integral
    
    def mdust(self, chain, dl, kappa=2.64, kappalambda=125.0,
              forcethin=False) :
        """Computes m_dust (in solar masses), taking an array of parameters
        in the order T, beta, alpha, lambda0, fnorm, and the luminosity
        distance in [m], and assuming the Dunne value for kappa_nu 
        (2.64 [m^2 kg^-1] at 125 [um] rest); this can be changed
        with kappa (in [m^2 kg^-1]) or kappalambda (in [microns] rest)"""
        #Using mks units in this function
        if self.redshift is None :
            raise ValueError("Redshift is not set in mdust")
        opz = 1.0+self.redshift
        dl2 = dl**2
        lamrest = self.wavenorm*1e-6/opz
        nurest  = 299792568/lamrest
        Tfac = 6.6260693e-34*nurest/1.38065e-23  #h nu / k
        bnufac = 2*6.6260693e-34*nurest**3/299792458.0**2 #Non-T dependent part
        knufac = lamrest / (kappalambda*1e-6) 
        nchain = chain.shape[0]
        msolar = 1.98892e30 ##kg
        dustmass = numpy.zeros(nchain) #in 10^8 solar masses
        if forcethin :
            #Force the optically thin assumption, even if the chain
            # doesn't match it
            for i in xrange(nchain) :
                pars = chain[i,:]
                T = pars[0]*opz
                beta = pars[1]
                fnorm = pars[4] 
                snu = fnorm*1e-29 #From mJy to W / m^2-Hz
                bnu = bnufac/(math.exp(Tfac/T)-1.0)
                knu = kappa * knufac ** (-beta) ##Can work in obs frame here
                dustmass[i]  = dl2*snu/(opz*knu*bnu*msolar)
        else :
            for i in xrange(nchain) :
                pars = chain[i,:]
                T = pars[0]*opz
                beta = pars[1]
                fnorm = pars[4] 
                snu = fnorm*1e-29
                bnu = bnufac/(math.exp(Tfac/T)-1.0)
                knu = kappa * knufac ** (-beta) 
                taunu = (pars[3]/wavenorm)**beta 
                opfac = taunu / ( 1.0 - math.exp(-taunu) )
                dustmass[i]  = dl2*snu*opfac/(opz*knu*bnu*msolar)
        return dustmass

if __name__ == "__main__" :
    #Do the fit!
    import argparse
    import os.path
    import pickle
    
    desc = """Fit a modified blackbody to user provided data using an MCMC"""
    epi = r"""The fit is done using rest frame quantities.  Input wavelengths
should be specified in microns and fluxes in mJy.

The model is (schematically)
    S_nu = (1-exp(-tau)) B_nu,
where B_nu is the Planck function, and tau is the optical depth, assumed
of the form
    tau = (nu/nu0)^beta.
The fit parameters are:
    T:  The observer frame temperature in [K] (Trest/(1+z))
    beta: The dust attenuation slope
    alpha: The blue side power law slope
    lambda0: The observer frame wavlength where the dust becomes optically
             thick (lambda0rest * (1+z)) in [um]
    fnorm: The normalization flux in [mJy] in the observer frame (usually at 500
           [um], but this can be changed with --wavenorm)
"""

    parser = argparse.ArgumentParser(description=desc,epilog=epi)
    parser.add_argument('photfile',action='store',
                        help="Text file holding photometry in microns, mJy, error")
    parser.add_argument('outfile',action='store',
                        help="File to pickle resulting chain to")
    parser.add_argument('-b','--burn',action='store',type=int,default=500,
                        help="Number of burn-in steps to do (def: 500)")
    parser.add_argument('-c','--covfile',action='store',
                        help="FITS file containing covariances (in mJy)",
                        default=None)
    parser.add_argument('-d','--dl',action='store',type=float,default=None,
                        help="Luminosity distance (in cm)")
    parser.add_argument('-e','--covextn',action='store',default=0,type=int,
                        help="Extension of FITS file to look for cov matrix in (Def: 0)")
    parser.add_argument('--fixT',action='store_true', default=None,
                        help="Fix T to initial value")
    parser.add_argument('--fixBeta',action='store_true', default=None,
                        help="Fix Beta to initial value")
    parser.add_argument('--fixAlpha',action='store_true', default=None,
                        help="Fix Alpha to initial value")
    parser.add_argument('--fixLambda0',action='store_true', default=None,
                        help="Fix Lambda0 to initial value")
    parser.add_argument('--fixFnorm',action='store_true', default=None,
                        help="Fix Fnorm to initial value")
    parser.add_argument('--getliragn',action='store_true',default=False,
                        help="Get the rest frame Lir AGN (22.5-122.5 um).  You must set the redshift and dl")
    parser.add_argument('--getlir',action='store_true',default=False,
                        help="Get the rest frame Lir (8-1000 um).  You must set the redshift and dl")
    parser.add_argument('--getdustmass',action='store_true',default=False,
                        help="Get the dust mass.  You must set the redshift and dl")
    parser.add_argument('--initT', action='store', type=float, default=10.0,
                        help="Initial T/(1+z)")
    parser.add_argument('--initBeta', action='store', type=float, default=2.0,
                        help="Initial beta")
    parser.add_argument('--initAlpha', action='store', type=float, default=4.0,
                        help="Initial alpha")
    parser.add_argument('--initLambda0', action='store', type=float, 
                        default=2500.0,
                        help="Initial Lambda0*(1+z)")
    parser.add_argument('--initFnorm', action='store', type=float, default=40.0,
                        help="Initial Fnorm")
    parser.add_argument('-k','--kappanu',action='store',type=float,
                        default=2.64,
                        help="Dust mass coefficient in m^2 kg^-1 (def: 2.64)")
    parser.add_argument("--kappalambda",action="store",type=float,default=125.0,
                        help="Rest frame wavelength (in um) at which kappanu is defined (def: 125)")
    parser.add_argument("--lowT",action='store',type=float,default=None,
                        help="Lower limit on T (Def:0)")
    parser.add_argument("--lowBeta",action='store',type=float,default=None,
                        help="Lower limit on beta (Def:0)")
    parser.add_argument("--lowAlpha",action='store',type=float,default=None,
                        help="Lower limit on alpha (Def:0)")
    parser.add_argument("--lowLambda0",action='store',type=float,default=None,
                        help="Lower limit on lambda0 (Def:0)")
    parser.add_argument("--lowFnorm",action='store',type=float,default=None,
                        help="Lower limit on fnorm (Def:0)")
    parser.add_argument('-p','--photdir',action='store',
                        help="Directory to look for files in",
                        default=None)
    parser.add_argument('-n','--nwalkers',action='store',type=int,
                        help="Number of walkers to use in MCMC (Def: 250)",
                        default=250)
    parser.add_argument('-N','--nsteps',action='store',type=int,
                        default=1000,
                        help="Number of steps to take per walker (Def: 1000)")
    parser.add_argument('--noalpha',action='store_true',default=False,
                        help="Do not use blue side power law in fit")
    parser.add_argument('--nlir',action="store",type=int,
                        default=None,
                        help="Number of steps to include in LIR, dust mass calculations (Def: All of them)")
    parser.add_argument('-t','--threads',action='store',type=int,default=1,
                        help="Number of threads to use (Def: 1)")
    parser.add_argument("--upT",action='store',type=float,default=None,
                        help="Upper limit on T")
    parser.add_argument("--upBeta",action='store',type=float,default=None,
                        help="Upper limit on beta")
    parser.add_argument("--upAlpha",action='store',type=float,default=None,
                        help="Upper limit on alpha")
    parser.add_argument("--upLambda0",action='store',type=float,default=None,
                        help="Upper limit on lambda0")
    parser.add_argument("--upFnorm",action='store',type=float,default=None,
                        help="Upper limit on fnorm")
    parser.add_argument("-v","--verbose",action="store_true",default=False,
                        help="Print status messages")
    parser.add_argument('-V','--version',action='version',
                        version='%(prog)s 1.0')
    parser.add_argument('-w','--wavenorm',action='store', 
                        type=float, default=500.0,
                        help="Observer frame wavelength of normalization (def: 500)")
    parser.add_argument('-z','--redshift',action='store',type=float,
                        help="Redshift of object (Def: 0)", default=None)

    results = parser.parse_args() #Runs on sys.argv by default

    if not results.photdir is None :
        photfile = os.path.join(results.photdir,results.photfile)
    else :
        photfile = results.photfile
        
    if not results.covfile is None :
        if not results.photdir is None :
            covfile = os.path.join(results.photdir,results.covfile)
        else : covfile = results.covfile
    else: covfile = None

    #This object handles all the calculations
    sed = ModifiedBlackbody(photfile,covfile=covfile,
                            covextn=results.covextn, 
                            wavenorm=results.wavenorm,
                            redshift=results.redshift,
                            noalpha=results.noalpha)

    
    #Set parameters fixed/limits if present
    if results.fixT:
        sed.fix_param('T')
    if results.fixBeta:
        sed.fix_param('beta')
    if results.noalpha or results.fixAlpha:
        sed.fix_param('alpha')
    if results.fixLambda0:
        sed.fix_param('lambda0')
    if results.fixFnorm:
        sed.fix_param('fnorm')
        
    #Lower limits
    if not results.lowT is None : 
        sed.set_lowlim('T',results.lowT)
    if not results.lowBeta is None : 
        sed.set_lowlim('beta',results.lowBeta)
    if not results.lowAlpha is None : 
        sed.set_lowlim('alpha',results.lowAlpha)
    if not results.lowLambda0 is None : 
        sed.set_lowlim('lambda0',results.lowLambda0)
    if not results.lowFnorm is None : 
        sed.set_lowlim('fnorm',results.lowFnorm)
    #Upper Limits
    if not results.upT is None : 
        sed.set_uplim('T',results.upT)
    if not results.upBeta is None : 
        sed.set_uplim('beta',results.upBeta)
    if not results.upAlpha is None : 
        sed.set_uplim('alpha',results.upAlpha)
    if not results.upLambda0 is None : 
        sed.set_uplim('lambda0',results.upLambda0)
    if not results.upFnorm is None : 
        sed.set_uplim('fnorm',results.upFnorm)

    #Set up sampler
    nwalkers = results.nwalkers
    if (nwalkers <= 0) :
        raise ValueError("Invalid (non-positive) nwalkers: %d" % nwalkers)

    #initial values -- T, beta, alpha, lambda0, fnorm
    #Should add way for user to specify these
    p0mn  = numpy.array([results.initT, results.initBeta, results.initAlpha,
                         results.initLambda0, results.initFnorm])
    p0var = numpy.array([2,0.2,0.3,100,5.0])
    if results.fixT:
        p0var[0] = 0.0
    if results.fixBeta:
        p0var[1] = 0.0
    if results.noalpha or results.fixAlpha:
        p0var[2] = 0.0
    if results.fixLambda0:
        p0var[3] = 0.0
    if results.fixFnorm:
        p0var[4] = 0.0
    npar = len(p0mn)
    p0 = [ numpy.random.randn(npar)*p0var + p0mn for i in xrange(nwalkers) ]

    #Create sampler
    sampler = emcee.EnsembleSampler(nwalkers, npar, sed,
                                    threads=results.threads)

    #Do burn-in
    if results.burn <= 0 :
        raise ValueError("Invalid (non-positive) number of burn in steps: %d" %
                         results.burn)
    if results.verbose : print "Doing burn in with %d steps" % results.burn
    pos, prob, state = sampler.run_mcmc(p0, results.burn)
    
    # Reset the chain to remove the burn-in samples.
    sampler.reset()

    if results.nsteps <= 0 :
        raise ValueError("Invalid (non-positive) number of steps: %d" % 
                         results.nsteps)
    if results.verbose : print "Main chain with %d steps" % results.nsteps
    st = sampler.run_mcmc(pos, results.nsteps, rstate0=state)

    if results.verbose :
        print "Mean acceptance fraction:", \
            numpy.mean(sampler.acceptance_fraction)
        try :
            acor = sampler.acor
            print "Autocorrelation time: "
            print " Number of steps %d should be larger than this" % \
                results.nsteps
            print "\tT:        %f" % acor[0]
            print "\tbeta:     %f" % acor[1]
            if not results.noalpha:
                print "\talpha:    %f" % acor[2]
            print "\tlambda0:  %f" % acor[3]
            print "\tfnorm:    %f" % acor[4]
        except ImportError :
            print "Unable to estimate autocorrelation time (acor not installed)"

    #Compute LIR/dust mass quantities if requested
    if (results.getliragn or results.getlir or results.getdustmass) :
        if sed.redshift is None :
            raise ValueError("Must set redshift if asking for LIR/dustmass")
        if sed.dl is None :
            raise ValueError("Must set dl (in cm) if asking for LIR/dustmass")

        nsteps = sampler.flatchain.shape[0]
        if results.nlir is None :
            lirchain = sampler.flatchain
        else :
            if results.nlir <= 0 :
                raise ValueError("Invalid (non-positive) nlir %d" % \
                                     results.nlir)
            if (results.nlir > nsteps) :
                errstr = "nlir (%d) is larger than number of steps (%d)" %\
                    (results.nlir,nsteps)
                raise ValueError(errstr)
            lirchain = sampler.flatchain[0:results.nlir]

        opz = 1.0 + sed.redshift
        #4*pi*dl^2/L_sun in cgs -- so the output will be in solar luminosities
        dlcorr = 4*math.pi*dl**2/3.839e33
        if results.getliragn :
            if results.verbose : print "Getting LIR (42.5-122.5)"
            minfreq = 299792458e-3/122.5/opz #GHz
            maxfreq = 299792458e-3/42.5/opz
            liragn = sed.freqint( lirchain, minfreq, maxfreq, step = 0.5 )
            liragn *= dlcorr
        if results.getlir :
            if results.verbose : print "Getting LIR (8-1000)"
            minfreq = 299792458e-3/1000/opz
            maxfreq = 299792458e-3/8.0/opz
            lir = sed.freqint( lirchain, minfreq, maxfreq, step = 0.25 )
            lir *= dlcorr
        if results.getdustmass :
            if results.verbose : print "Getting dust mass"
            if results.kappanu <= 0 :
                raise ValueError("Invalid (non positive) kappa_nu %f" % 
                                 results.kappanu )
            if results.kappalambda <= 0 :
                raise ValueError("Invalid (non positive) kappalambda %f" % 
                                 results.kappalambda )
            #dustmass already returns solar masses, but takes dl in m
            dustmass = sed.mdust(lirchain,dl*1e-3,kappa=results.kappanu,
                                 kappalambda=results.kappalambda)

    #Save results
    if results.verbose : print "Saving results"
    output = open(results.outfile,'wb')
    chn = sampler.flatchain
    pickle.dump(chn,output)
    if results.getliragn :
        pickle.dump(liragn,output)
    if results.getlir :
        pickle.dump(lir,output)
    if results.getdustmass :
        pickle.dump(dustmass,output)
    output.close()

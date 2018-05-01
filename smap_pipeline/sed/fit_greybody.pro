;+
;NAME
; fit_greybody
;PURPOSE
; Does a chi^2 minimization fit of a greybody to
; a set of input photometry.
;USAGE
; fit_greybody, datafile
;INPUTS
; datafile      Input file of data points giving wavelength, f_nu,
;                error, with wavelength in microns
;OPTIONAL INPUTS
; init_lambda0    Initial guess for lambda0*(1+z) in um (def: 300 um)
; init_beta       Initial guess for beta (def: 1.7)
; init_T          Initial guess for T/(1+z) (def: 30)
; init_alpha      Initial guess for alpha (def: 1.5)
; init_f500       Initial guess at 500 micron flux (def: estimated)
; covfile         File (FITS) of covariances between data points.
;                  If present, error in datafile is completely ignored
; ex_covfile      The FITS extension to look for the covariance in
;                    (def: 0)
; fudgefac        Scale up errors on photometry by this amount.
;KEYWORDS
; simple          Use simplified greybody model -- see make_greybody_sed.
;                  lambda0 is not fit in this case
; fixalpha        Fix alpha in fit at init_alpha.
; fixbeta         Fix beta in fit at init_beta.
; fixT            Fix T/(1+z) in fit at init_T.
; fixlambda0      Fix lambda0*(1+z) in fit at init_lambda0.
; fixf500         Fix f500 in fit at init_f500
; noalpha         Do not use high frequency alpha merge.
; quiet           Don't summarize fit to the terminal.
;OPTIONAL OUTPUTS
; values          Values of params in lambda0,beta,T,f500,alpha order
; perrors         Errors in values
; covar           Covariance matrix of values
; chisq           Chi^2 of best fit
;MODEL
; The model fit to the data is a graybody of the form
;    F_nu = A ( 1 - EXP( -(lambda0/lambda)^beta ) )  B_nu(T)
; Where B_nu is the Planck function
; The model paramters are:
;     A (overall normalization)
;     lambda0 (greybody frequency parameter)
;     beta (greybody slope parameter)
;     T (temperature)
; This is (potentially) joined to a nu^(-alpha) power law at high
;  frequencies unless /NOALPHA is set.
; A is actually set up to be the flux at 500 microns rather than a
;  pure multiplication in order to reduce parameter covariances.
; If /SIMPLE is set the model becomes
;     F_nu = A lambda^-beta B_nu(T)
;NOTES
; Fitting for alpha may destabalize the code; the joining of
; the power law involves numeric root finding which may not work
; in practice.  An error message related to FX_ROOTS implies that the
; alpha joining routine failed.
;REDSHIFT DEPENDENCE
; The model is completely degenerate in 1+z if T is interpreted
; as T/(1+z) and lambda0 as lambda0*(1+z).  Therefore, one should really
; think of these as the parameters.
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 17, 2010
;         Mike Zemcov, Nov 4, 2011, changed some of Alex's defaults to 
;                                   make life better for all of us.
;-

FUNCTION fit_greybody_fitfun, p, NOALPHA=noalpha, SIMPLE=simple,$
                               LAMBDA_FIX=lambda_fix

  COMMON fit_greybody_common, datawave, dataflux, dataerror, ivars,$
     have_cov, evecs
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;;Get unnormalized model
  IF KEYWORD_SET( noalpha ) THEN BEGIN
     model_flux = make_greybody_sed( datawave, LAMBDA0=p[0],$
                                      BETA=p[1],T=p[2], F500=p[3],$
                                      SIMPLE=simple, LAMBDA_FIX=lambda_fix )
  ENDIF ELSE BEGIN
     model_flux = make_greybody_sed( datawave, LAMBDA0=p[0],$
                                      BETA=p[1], T = p[2], F500=p[3],$
                                      ALPHA=p[4], SIMPLE=simple,$
                                      LAMBDA_FIX=lambda_fix)
  ENDELSE

  ;;Transform to diagonal frame. dataflux has
  ;; already been transformed if needed
  IF have_cov THEN model_flux = REFORM( evecs ## model_flux )

  RETURN,(dataflux-model_flux)/dataerror
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO fit_greybody, datafile, INIT_LAMBDA0=init_lambda0, INIT_BETA=init_beta,$
                   INIT_T=init_T, INIT_ALPHA=init_alpha, INIT_F500=init_f500,$
                   COVFILE=covfile, EX_COVFILE=ex_covfile, SIMPLE=simple, $
                   FIXALPHA=fixalpha, FIXBETA=fixbeta, FIXT=fixt,$
                   FIXLAMBDA0=FIXLAMBDA0, FIXF500=fixf500, NOALPHA=noalpha, $
                   VALUES=values, PERRORS=perrors, COVAR=covar, QUIET=quiet, $
                   CHISQ=chisq, FUDGEFAC=fudgefac, LAMBDA_FIX=lambda_fix

  ;;An evil common block works better here than extra_info because
  ;; of the options
  COMMON fit_greybody_common, datawave, dataflux, dataerror, ivars,$
     have_cov, evecs

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(init_lambda0) EQ 0 THEN init_lambda0 = 300.0
  IF N_ELEMENTS(init_beta) EQ 0 THEN init_beta = 1.7
  IF N_ELEMENTS(init_T) EQ 0 THEN init_T = 30
  IF N_ELEMENTS(init_alpha) EQ 0 THEN init_alpha=1.5
  IF N_ELEMENTS(lambda_fix) EQ 0 THEN lambda_fix = 500.0d0

  ;;Read data file
  IF ~ FILE_TEST( datafile ) THEN MESSAGE,"Can't find datafile: "+datafile
  IF ~ FILE_TEST( datafile, /READ ) THEN $
     MESSAGE,"Can't read datafile: "+datafile
  READCOL,datafile,wavelength,flux,flux_error,FORMAT='(F,F,F)',$
          /SILENT
  ndata = N_ELEMENTS(wavelength)
  IF ndata EQ 0 THEN MESSAGE,"No data read from: "+datafile
  IF MIN(flux_error) LE 0.0 THEN MESSAGE,"Invalid errors"
  datawave = TEMPORARY(wavelength)
  raw_dataflux = flux
  dataflux = TEMPORARY(flux)

  ;;Set up covariance info
  IF N_ELEMENTS( covfile ) EQ 0 THEN BEGIN
     have_cov = 0b
     dataerror = TEMPORARY( flux_error )
     IF N_ELEMENTS(fudgefac) NE 0 THEN dataerror *= fudgefac[0]
     ivars = 1.0/dataerror^2
  ENDIF ELSE BEGIN
     have_cov = 1b
     IF ~ FILE_TEST( covfile ) THEN MESSAGE,"Can't find covariance file: "+$
                                            covfile
     IF ~ FILE_TEST( covfile, /READ ) THEN $
        MESSAGE,"Can't find covariance file: "+covfile
     IF N_ELEMENTS(ex_covfile) EQ 0 THEN ex_covfile=0
     cov = MRDFITS( covfile, ex_covfile, STATUS=status, /SILENT )
     IF status NE 0 THEN MESSAGE,"Error reading covariance file: "+covfile
     szcov = SIZE(cov)
     IF szcov[0] NE 2 THEN MESSAGE,"Cov matrix not 2D"
     IF szcov[1] NE szcov[2] THEN MESSAGE,"Cov matrix not square"
     IF szcov[1] NE ndata THEN MESSAGE,"Cov matrix doesn't match data file size"
     
     IF N_ELEMENTS(fudgefac) NE 0 THEN cov *= fudgefac[0]^2

     inv_cov = LA_INVERT( cov, STATUS=status, /DOUBLE )
     IF status NE 0 THEN MESSAGE,"Error inverting covariance matrix"

     ;;Now we have to do some variable transform stuff to use MPFIT,
     ;; which only really supports diagonal covariance matrices.
     ;;We use the usual trick:
     ;; invC = A^T invC' A
     ;;where invC' is the diagonal matrix of eigenvalues and
     ;; A is the matrix of eigenvectors
     ;;You then use A to transform the data point differences
     evals = LA_EIGENQL( inv_cov, /DOUBLE, EIGENVECTORS=evecs,$
                         STATUS=status)
     IF status NE 0 THEN MESSAGE,"Error diagonalizing inverse cov matrix"
     IF MIN(evals LE 0.0) THEN $
        MESSAGE,"Invalid covariance matrix -- has 0 or negative eigenvalue"

     ivars = evals
     dataerror = 1.0/SQRT(evals)
     dataflux = REFORM( evecs ## dataflux )
  ENDELSE

  ;;Set up extra stuff for fit: limits, optional vars, etc.
  extra_info = { noalpha: 0b, simple: 0b, lambda_fix: lambda_fix }
  extra_info.noalpha = KEYWORD_SET( noalpha )
  extra_info.simple = KEYWORD_SET( simple )
  param_info=REPLICATE( {parname:'', fixed:0, limited:[0,0], $
                         limits:[0.d0,0.d0], step: 0.0, mpside: 0,$
                         mpmaxstep: 0.0}, 5)
  param_info.parname = ['lambda0','beta   ','T      ','f500   ','alpha  ']
  IF KEYWORD_SET( fixlambda0 ) OR KEYWORD_SET( simple ) THEN $
     param_info[0].fixed = 1b
  IF KEYWORD_SET( fixbeta ) THEN param_info[1].fixed = 1b
  IF KEYWORD_SET( fixt ) THEN param_info[2].fixed = 1b
  IF KEYWORD_SET( fixf500 ) THEN param_info[3].fixed = 1b
  IF KEYWORD_SET( noalpha ) OR KEYWORD_SET( fixalpha ) THEN $
     param_info[4].fixed = 1b
  param_info.step = [1,0.001,0.001,0.1,0.001]
  param_info[0].limited = [1,0]
  param_info[0].limits  = [1,0]
  param_info[0].mpside = 2
  param_info[0].mpmaxstep = 10
  param_info[1].limited = [1,1]
  param_info[1].limits  = [0.0, 10.0]
  param_info[2].limited[0] = 1
  param_info[2].limits[0] = 1d-5
  param_info[3].limited[0] = 1
  param_info[3].limits[0] = 0.0
  param_info[4].limited = [1,1]
  param_info[4].limits  = [0.0, 10.0]

  ;;Now, get rough guess at normalization
  IF N_ELEMENTS(init_f500) EQ 0 THEN BEGIN
     IF KEYWORD_SET( noalpha ) THEN BEGIN
        init_model_flux = make_greybody_sed([datawave,500], $
                                             LAMBDA0=init_lambda0,$
                                             BETA=init_beta,T=init_T, $
                                             SIMPLE=simple,$
                                             LAMBDA_FIX=lambda_fix)
     ENDIF ELSE BEGIN
        init_model_flux = make_greybody_sed([datawave,500], $
                                             LAMBDA0=init_lambda0,$
                                             BETA=init_beta,T=init_T, $
                                             ALPHA=init_alpha, SIMPLE=simple,$
                                             LAMBDA_FIX=lambda_fix)
     ENDELSE
     init_f500 = init_model_flux[ndata] * $
                 MEAN(raw_dataflux / init_model_flux[0:ndata-1],/NAN)
  ENDIF
  
  start_values = [init_lambda0, init_beta, init_t, init_f500, init_alpha]

  values = MPFIT( 'fit_greybody_fitfun', start_values, $
                  FUNCTARGS=extra_info, MAXITER=1000,$
                  ERRMSG=errmsg, STATUS=status,$
                  COVAR=covar, PERROR=perrors, BESTNORM=chisq,$
                  PARINFO=param_info,/QUIET)
  IF status EQ -18 THEN BEGIN
     f500 = !VALUES.F_NAN
     status = 1
  ENDIF
  IF status LE 0 THEN BEGIN
     MESSAGE,"Error performing fit: " + errmsg,/INFORMATIONAL
     values = REPLICATE(!VALUES.F_NAN,N_ELEMENTS(start_values))
     perrors = FLTARR(N_ELEMENTS(start_values))
  ENDIF
  IF status EQ 5 THEN $
     PRINT,"Maximum number of iterations exceeded in fit_greybody"

  IF ~ KEYWORD_SET( quiet ) AND FINITE(values[0]) THEN BEGIN
     IF ~ KEYWORD_SET( simple ) THEN BEGIN
        IF param_info[0].fixed THEN $
           PRINT,"lambda0*(1+z): ",values[0],$
                 FORMAT='(A14,2X,F7.2," (fixed)")' ELSE $
              PRINT,"lambda0*(1+z): ",values[0],perrors[0],$
                    FORMAT='(A14,2X,F7.2," +- ",E7.2)'
     ENDIF
     IF param_info[1].fixed THEN $
           PRINT,"beta: ",values[1],FORMAT='(A14,2X,F7.3," (fixed)")' ELSE $
              PRINT,"beta: ",values[1],perrors[1],$
                    FORMAT='(A14,2X,F7.3," +- ",F7.3)'
     IF param_info[2].fixed THEN $
           PRINT,"T/(1+z): ",values[2],FORMAT='(A14,2X,F7.3," (fixed)")' ELSE $
              PRINT,"T/(1+z): ",values[2],perrors[2],$
                    FORMAT='(A14,2X,F7.3," +- ",F7.3)'
     fstr = STRING(lambda_fix,FORMAT='("f",I0)')
     IF param_info[3].fixed THEN $
           PRINT,fstr+": ",values[3],FORMAT='(A14,2X,F7.3," (fixed)")' ELSE $
              PRINT,fstr+": ",values[3],perrors[3],$
                    FORMAT='(A14,2X,F7.3," +- ",F7.3)'
     IF ~ KEYWORD_SET( noalpha ) THEN BEGIN
        IF param_info[4].fixed THEN $
           PRINT,"alpha: ",values[4],FORMAT='(A14,2X,F6.3," (fixed)")' ELSE $
              PRINT,"alpha: ",values[4],perrors[4],$
                    FORMAT='(A14,2X,F6.3," +- ",F6.3)'
     ENDIF
        
     wfree = WHERE( ~ param_info.fixed, nfree )
     PRINT,chisq,N_ELEMENTS(dataflux)-nfree,$
           FORMAT='("Minimum chisquare: ",F0.2," for ",I0," dof")'
  ENDIF

END

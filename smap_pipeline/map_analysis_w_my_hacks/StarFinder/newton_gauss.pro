; $Id: newton_gauss.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	NEWTON_GAUSS
;
; PURPOSE:
;	Optimize the parameters of parametric model in order to fit a set
;	of measurements, by means of the Newton-Gauss iterative algorithm.
;	This method requires the evaluation of both the model and its
;	derivatives with respect to the parameters.
;
; CATEGORY:
;	Mathematics. Optimization.
;
; CALLING SEQUENCE:
;	NEWTON_GAUSS, Model_fun, Iacobi_fun, Converg_fun, Ini_x, Data, $
;				  Converging, X, Sigma_x, Model
;
; INPUTS:
;	Model_fun:	Name of a function to compute the parametric model.
;		It must be defined as follows:
;		FUNCTION Model_fun, X, KEYWORDS = keywords
;		where X is a set of parameters and KEYWORDS represents a list
;		of input keyword parametres. The result of Model_fun must have
;		the same size as the data to be approximated (see below)
;
;	Iacobi_fun:	Name of a function to compute the derivatives of the model.
;		It must be defined as follows:
;		FUNCTION Iacobi_fun, X, KEYWORDS = keywords
;		where X is a set of parameters and KEYWORDS represents a list
;		of input keyword parametres. The result of Iacobi_fun must be an
;		array of size N*M, where N (number of columns) is the number of
;		parameters and M (size of rows) is the number of elements in the
;		input data. The element [n,m] of the result is the derivative of
;		the model with respect to the n-th parameter, computed at the m-th
;		point
;
;	Converg_fun:	Name of a function to check the convergence condition
;		between to sets of parameters, corresponding to successive iterations.
;		It must be defined as follows:
;		FUNCTION Converg_fun, X0, X, KEYWORDS = keywords
;		where X0 and X are two sets of parameters and KEYWORDS represents a
;		list of input keyword parametres. It must return a logical value
;
;	Ini_x:	Initial guess of the set of parameters
;
;	Data:	Data to be approximated with the parametric model
;
; KEYWORD PARAMETERS:
;	BAD_DATA:	Array of subscripts (compatible with the input Data),
;		representing Data values to be masked
;
;	MASK:	Set this keyword to a nonzero value to apply the data masking
;		mode no. 2 described in PROCEDURE description, provided an estimate
;		of the background noise standard deviation is supplied on input
;		(see next keyword)
;
;	NOISE_STD:	Estimate of the noise standard deviation. It may be a scalar
;		("white noise" case) or an array, having the same size as Data
;
;	NOISE_TOL:	Fix a threshold for bad data identification in data masking
;		mode no. 2. See PROCEDURE description. The default is NOISE_TOL = 5
;
;	WHEN:	Iteration number when to identify bad data in data masking mode
;		no. 2. See PROCEDURE description. The default is WHEN = 2 (i.e.
;		2nd iteration)
;
;	SCALE:	Set this keyword to a nonzero value to scale the set of variables
;		(model parameters), in order to prevent noise amplification
;
;	MAXIT:	Maximum number of iteration allowed. The default is 30.
;
;	INVERSE_DATA_VAR:	Inverse noise variance on input Data. This parameter
;		may be either a scalar ("white noise" case) or an array, having the
;		same size as Data.
;
;	_EXTRA:	Extra keywords which are accepted by NEWTON_GAUSS and passed
;		directly to the functions Model_fun, Iacobi_fun and Converg_fun.
;		These extra keywords may be used to handle global data, as an
;		alternative to COMMON blocks.
;
; OUTPUTS:
;	Converging:	Logical value, true if the algorithm has converged,
;		according to the convergence condition defined by the function
;		Converg_fun
;
;	X:	Optimal set of parameters
;
; OPTIONAL OUTPUTS:
;	Sigma_x:	Formal errors (at "1 sigma level") on the estimated
;		parameters. Available only if INVERSE_DATA_VAR has been provided
;
;	Model:	Best fit model
;
;	IT:	Use this output keyword to monitor the actual number of iterations
;		performed
;
;	W_BAD:	Use this output keyword to retrieve the subscripts of the bad
;		data which have been masked. The value of this keyword concide with
;		the input BAD_DATA if only data masking mode no. 1 is performed
;
; PROCEDURE:
;	The problem may be expressed mathematically as a set of non-linear
;	algebraic equations, in the form
;	Model(X) - Data = 0.
;	The algebraic system is solved by means of the iterative Newton-Gauss
;	scheme
;	M Dx = Data - Model(X), X' = X + Dx
;	where M is the Iacobi matrix of the model with respect to the parameters,
;	X is the old set of parameters, Dx a correction and X' a new estimate.
;	At every iteration the correction vector Dx is found as the minimum norm
;	solution of the linear system
;	M Dx = Data - Model
;	The vector of parameters X may be scaled (see keyword SCALE), in order to
;	prevent noise amplification in the inversion of the matrix, due to ill
;	conditioning.
;	The algorithm requires an initial estimate of the solution (Ini_x) and a
;	stopping criterion (e.g. variation of parameters smaller than a pre-fixed
;	tolerance or upper threshold on number of iterations).
;	Bad data masking may be performed by:
;	1) providing an array of subscripts for the points to be masked
;	2) providing an estimate of the background noise standard deviation (see
;	keyword NOISE_STD), used by the algorithm to identify the points where
;	the error between the Data and the Model (computed at the iteration
;	specified by the keyword WHEN) is larger than  NOISE_TOL * NOISE_STD.
;	The two data masking mode may be used together.
;	It is possible to weigh the data by the inverse standard deviation of
;	the error on each value (see the keyword INVERSE_DATA_VAR)
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO newton_gauss, model_fun, iacobi_fun, converg_fun, ini_x, data, $
   		  BAD_DATA = bad_data, MASK = mask, NOISE_STD =	noise_std, $
   		  NOISE_TOL = noise_tol, WHEN = mask_now, SCALE = scale,   $
   		  MAXIT = maxit, INVERSE_DATA_VAR = inverse_var, $
   		  converging, x, sigma_x, model, IT = it, W_BAD = w_bad, $
   		  _EXTRA = extra

	on_error, 2
	if  n_elements(bad_data) ne 0  then  w_bad = bad_data
	mask = keyword_set(mask) and n_elements(noise_std) ne 0
	if  mask  then begin
	  if  n_elements(mask_now) eq 0  then  mask_now = 2
	  if  n_elements(noise_tol) eq 0  then  noise_tol = 5
	endif
	if  n_elements(maxit) eq 0  then  maxit = 30
	n_data = n_elements(data)
	if  n_elements(inverse_var) eq 1  then $
	   inverse_var = replicate(inverse_var, n_data)
	it = 0L  &  x = ini_x
	model = call_function(model_fun, x, _EXTRA = extra)
	converging = 0B

	while  it lt maxit and not converging  do begin

	   it = it + 1
	   if  mask  then  if  it eq mask_now  then  w_bad = $
	      append_elements(w_bad, where(abs(data - model) gt noise_tol*noise_std ))
	   iacobi = call_function(iacobi_fun, x, _EXTRA = extra)
	   ls_sys, iacobi, reform(data - model, n_data), MASK = w_bad, $
	   		   WEIGHTS = inverse_var, lin_hessian, grad
	   if  keyword_set(scale)  then $
	      scale_ls_sys, lin_hessian, grad, lin_hessian, grad, s, $
	      				NOCOMP = (it gt 1) and 1B
	   dx = min_norm_inversion(lin_hessian, grad, SCALING = s, $
	   						   INVERSE = inv_hessian)
	   x0 = x  &  x = x + dx
	   model = call_function(model_fun, x, _EXTRA = extra)
	   converging = call_function(converg_fun, x0, x, _EXTRA = extra)

	endwhile

	if  converging  then  if  n_elements(inverse_var) ne 0  then $
	   sigma_x = fitting_errors(inv_hessian, SCALING = s)

	return
end

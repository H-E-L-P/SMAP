;+
; NAME:
;	ELECTRON_MFP
;
; PURPOSE:
; 
;	This function returns the elastic mean-free-path for electrons
;	of energy E, in a material having density N, and atomic number Z.
;
; CALLING SEQUENCE:
; 
;	Result = ELECTRON_MFP(Z,A,RHO,E)
;
; INPUTS:
;	
;	Z = Atomic number
;	
;	A = Atomic weight (g/mole)
;	
;	RHO = Density (g/cm3)
;	
;	E = electron energy in keV
;
; OUTPUTS:
; 
;	This function returns the elastic mean-free-path, in angstroms.
;
; EXAMPLE:
;
;       The elastic mean-free-path of tungsten (Z=74, Rho=19.35) at an
;       electron energy of 100 keV = ELECTRON_MFP(74,183.85,19.35,100.)
;
; MODIFICATION HISTORY:
; 
;	Written by D. L. Windt, Bell Labs, June 1994
;	windt@bell-labs.com
;-

function electron_mfp,z,a,rho,e

if n_params() ne 4 then message,'Usage: Result=ELECTRON_MFP(Z,AT_WT,RHO,E)'

e_0=511.
a_0=6.02e23
n=rho*a_0*1.e-24/a
return,1./(n * (1.9e-4) * z^(4./3.) * (e_0+e)^2 / ( (e_0+e)^2-e_0^2) )
end


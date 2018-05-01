;+
; NAME:
;
;      ATOMIC_WEIGHT
;
; PURPOSE:
;
;      Function to return the atomic weight of specified chemical
;      elements.
;
; CALLING SEQUENCE:
;
;      Result=ATOMIC_WEIGHT(SYMBOL)
; 
; INPUTS:
;
;      SYMBOL - A string or string array specifying the name or names
;               of the chemical elements. Each element of SYMBOL must
;               be a one or two character string, corresponding to the
;               chemical symbol of the atom.  Case is ignored.
;
; KEYWORD PARAMETERS:
;
;      ALL - Set this to return all 92 atomic weights and symbols.
;
; OUTPUTS:
;
;      Result - The atomic weight of the specified atom or atoms.
;
; RESTRICTIONS:
;
;      Only the first 92 elements are available.
;
; PROCEDURE:
;
;      The mass of the proton is first calculated using quantum field
;      theory, and then...actually, it's just a lookup table.
;
; EXAMPLE:
;
;      Print the atomic weight of carbon:
;
;      print,ATOMIC_WEIGHT('C')
;
; MODIFICATION HISTORY:
;
;      David L Windt, Bell Labs, May 1997
;      windt@bell-labs.com
;
;-
function atomic_weight,symbol,all=all

on_error,2

if n_params() ne 1 then message,'Usage: Result=ATOMIC_WEIGHT(SYMBOL)'

symbol_list=[ $
              'Ac', $
              'Ag', $
              'Al', $
              'Ar', $
              'As', $
              'At', $
              'Au', $
              'B', $
              'Ba', $
              'Be', $
              'Bi', $
              'Br', $
              'C', $
              'Ca', $
              'Cd', $
              'Ce', $
              'Cl', $
              'Co', $
              'Cr', $
              'Cs', $
              'Cu', $
              'Dy', $
              'Er', $
              'Eu', $
              'F', $
              'Fe', $
              'Fr', $
              'Ga', $
              'Gd', $
              'Ge', $
              'H', $
              'He', $
              'Hf', $
              'Hg', $
              'Ho', $
              'I', $
              'In', $
              'Ir', $
              'K', $
              'Kr', $
              'La', $
              'Li', $
              'Lu', $
              'Mg', $
              'Mn', $
              'Mo']
symbol_list=[symbol_list, $
             'N', $
             'Na', $
             'Nb', $
             'Nd', $
             'Ne', $
             'Ni', $
             'O', $
             'Os', $
             'P', $
             'Pa', $
             'Pb', $
             'Pd', $
             'Pm', $
             'Po', $
             'Pr', $
             'Pt', $
             'Ra', $
             'Rb', $
             'Re', $
             'Rh', $
             'Rn', $
             'Ru', $
             'S', $
             'Sb', $
             'Sc', $
             'Se', $
             'Si', $
             'Sm', $
             'Sn', $
             'Sr', $
             'Ta', $
             'Tb', $
             'Tc', $
             'Te', $
             'Th', $
             'Ti', $
             'Tl', $
             'Tm', $
             'U', $
             'V', $
             'W', $
             'Xe', $
             'Y', $
             'Yb', $
             'Zn', $
             'Zr']

atwt_list=[ $
            227.028, $
            107.868, $
            26.9814, $
            39.948, $
            74.9216, $
            210., $
            196.9665, $
            10.81, $
            137.33, $
            9.01218, $
            208.9804, $
            79.904, $
            12.011, $
            40.08, $
            112.41, $
            140.12, $
            35.453, $
            58.9332, $
            51.996, $
            132.9054, $
            63.546, $
            162.50, $
            167.26, $
            151.96, $
            18.998403, $
            55.847, $
            223., $
            69.72, $
            157.25, $
            72.59, $
            1.0079, $
            4.0026, $
            178.49, $
            200.59, $
            164.9304, $
            126.9045, $
            114.82, $
            192.22, $
            39.0983, $
            83.80, $
            138.9055, $
            6.941, $
            174.967, $
            24.305, $
            54.9380, $
            95.94]
atwt_list=[atwt_list, $
           14.0067, $
           22.98977, $
           92.9064, $
           144.24, $
           20.179, $
           58.7, $
           15.9994, $
           190.2, $
           30.97376, $
           231.0359, $
           207.2, $
           106.4, $
           145., $
           209., $
           140.907, $
           195.09, $
           222., $
           85.4678, $
           186.2, $
           102.9055, $
           222., $
           101.07, $
           32.06, $
           121.75, $
           44.9559, $
           78.96, $
           28.0855, $
           150.4, $
           118.69, $
           87.62, $
           180.9479, $
           158.9254, $
           97., $
           127.6, $
           204.37, $
           47.9, $
           204.37, $
           168.9342, $
           238.029, $
           50.9415, $
           183.85, $
           131.3, $
           88.9059, $
           173.04, $
           65.38, $
           91.22]

;; return all?
if keyword_set(all) then begin 
    symbol=symbol_list
    atwt=atwt_list
endif else begin
    atwt=fltarr(n_elements(symbol))
    for i=0,n_elements(symbol)-1 do begin
        index=where(strtrim(strlowcase(symbol(i)),2) eq  $
                    strtrim(strlowcase(symbol_list),2))
        if index(0) eq -1 then $
          message,'Invalid element: '+symbol(i)+'.'
        atwt(i)=atwt_list(index)
    endfor
endelse
return,atwt
end


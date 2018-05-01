;+
; MAKE_ALL_HEDAM_FIELDS
;
; Wrapper script for MAKE_HEDAM_FIELD. Contains specific information
; about fields and data release.
;
; CREATED BY: G. Marsden, 20110318
;
; CHANGELOG:
;   20110318,GM: data release v3.0
;   20110505,GM: add nest and filter fields
;   20110715,GM: update for v4.0
;   20110909,GM: update for v4.1
;   20111215,AC: update for v4.2
;   20111215,GM: re-update for v4.2
;                move "tempmaskdir" out of make_hedam_field.pro
;   20131001,AC: update for v6.0, removes some fields
;-

PRO MAKE_ALL_HEDAM_FIELDS

  COMPILE_OPT IDL2
  inbasedir  = '/data/spire/maps'
  outbasedir = '/data/spire/release'
  version = 'v6.0'
  tempmaskdir = '/data/spire/maps/tempmasks/20131002/'

  CALDAT, SYSTIME(/JUL), m, d, y
  todaystr = STRING([y,m,d], FORM='(I04,I02,I02)')

; list of processing dates used in map creation
  date1 = '20111129'
  date2 = '20111130'
  date3 = '20111201'
  date4 = '20111202'
  date5 = '20111207'
  date6 = '20111208'
  date7 = '20111214'
  date8 = '20120125'
  date9 = '20120126'
  date10 = '20120127'
  date11 = '20130906'
  date12 = '20131002'
  date13 = '20130811'
  date14 = '20130809'
  date15 = '20130810'
  date16 = '20131101'
  nodate= 'nodate'

; map data.  Note that some fields (which HeDAM doesn't distribute)
; are not included
  fields = [ $
           {f:"abell0370",     d:date1, n:"Abell-370"}, $
           {f:"abell1689",     d:date1, n:"Abell-1689"}, $
           {f:"abell1835",     d:date1, n:"Abell-1835"}, $
           {f:"abell2218",     d:date1, n:"Abell-2218"}, $
           {f:"abell2219",     d:date1, n:"Abell-2219"}, $
           {f:"abell2390",     d:date1, n:"Abell-2390"}, $
           {f:"adfs",          d:date11, n:"ADFS"}, $
           {f:"bootes",        d:date1, n:"BOOTES-HerMES"}, $
           {f:"cdfs-shallow",  d:date12, n:"CDFS-SWIRE"},$ 
           {f:"cl0024",        d:date1, n:"Cl0024+16"}, $
           {f:"ecdfs",         d:date1, n:"ECDFS"}, $
           {f:"egroth",        d:date1, n:"Groth-Strip"}, $
           {f:"egs-scuba",     d:date1, n:"EGS-HerMES"}, $
           {f:"elais-n2",      d:date1, n:"ELAIS-N2-HerMES"}, $
           {f:"elais-s1",      d:date1, n:"ELAIS-S1-SWIRE"}, $
           {f:"fls",           d:date1, n:"FLS"}, $
           {f:"goodsn",        d:date1, n:"GOODS-N"}, $
           {f:"goodsn-gh",     d:date8, n:"GOODS-N2"}, $
           {f:"goodss",        d:date1, n:"GOODS-S"}, $
           {f:"lockman-east",  d:date1, n:"Lockman-East-ROSAT"}, $
           {f:"lockman-north", d:date1, n:"Lockman-North"}, $
           {f:"lockman-shallow", d:date12, n:"Lockman-SWIRE"},$
           {f:"ms0451",        d:date1, n:"MS0451.6-0305"}, $
           {f:"ms1054",        d:date1, n:"MS1054.4-0321"}, $
           {f:"ms1358",        d:date16, n:"MS1358+62"}, $
           {f:"rxj0152",       d:date1, n:"RXJ0152.7-1357"}, $
           {f:"rxj1347",       d:date1, n:"RXJ13475-1145"}, $
           {f:"xmm-lss",       d:date1, n:"XMM-LSS-SWIRE"}, $
           {f:"cdfs-nest",     d:date14, n:"CDFS-SWIRE-Nest"}, $
           {f:"cosmos-nest",   d:date13, n:"COSMOS-Nest"},$
           {f:"egs-nest",      d:date1, n:"EGS-HerMES-Nest"}, $
           {f:"elais-n1-nest", d:date3, n:"ELAIS-N1-HerMES-Nest"}, $
           {f:"goodsn-nest",   d:date14, n:"GOODS-N-Nest"}, $
           {f:"lockman-nest",  d:date15, n:"Lockman-SWIRE-Nest"}, $
           {f:"xmm-nest",      d:date11, n:"XMM-LSS-SWIRE-Nest"}, $
           {f:"cosmos-filter", d:date3, n:"COSMOS-Filter"}, $
           {f:"egroth-filter", d:date2, n:"Groth-Strip-Filter"}, $
           {f:"goodsn-filter", d:date2, n:"GOODS-N-Filter"}, $
           {f:"goodss-filter", d:date4, n:"GOODS-S-Filter"}, $
           {f:"lockman-east-filter", d:date10, n:"Lockman-East-ROSAT-Filter"}, $
           {f:"lockman-north-filter", d:date6, n:"Lockman-North-Filter"}, $
           {f:"uds-filter",    d:date6, n:"UDS-Filter"}, $
           {f:"vvds-filter",   d:date6, n:"VVDS-Filter"} $
           ]

  FOR f=0,N_ELEMENTS(fields)-1 DO BEGIN
     MESSAGE, STRING(fields[f].n, FORMAT='("Doing field: ",A0)'),/INF
     IF fields[f].d EQ nodate THEN $
        MESSAGE,"No date set for field: "+fields[f].f
     imapdir = ADDSLASH(inbasedir) + ADDSLASH(fields[f].f) + $
               ADDSLASH(fields[f].d)
     prefix  = fields[f].f + "_itermap_" + fields[f].d
     omapdir = ADDSLASH(outbasedir) + ADDSLASH(todaystr) + ADDSLASH(fields[f].f)

     MAKE_HEDAM_FIELD, imapdir, prefix, fields[f].n, omapdir, version, $
                       tempmaskdir
  ENDFOR



END

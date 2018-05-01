;;This is a template IDL startup file.

;;You will need to edit this to reflect your directory structure --
;; where you keep your data, and where you keep your code.

;;Once you edit it (and add any extra things you want), you should
;; save it by another name (for example: as ~/.idl_setup.pro).
;;Then you need to set the environment variable IDL_STARTUP to point
;; to the edited file.
;;So, if you edit and name it ~/.idl_setup.pro then you want to
;; add a line like
;;
;;export IDL_STARTUP=~/.idl_setup.pro
;;
;; to your login script (assuming you use bash shell -- for others,
;; substitute the apropriate way to set environment variables 
;; In tcsh this would be 
;;    setenv IDL_STARTUP ~/.idl_setup.pro


;;First, you need to define the path where you store SPIRE data
;; This is an example.  Make sure it ends in the path seperator!
defsysv,'!SMAP_DATA','/data/viero/data/'
;; Now let's define the path to SPIRE source catalogs
defsysv,'!SMAP_CATS','/data/viero/cats/'
;; and finally the path to SPIRE maps
defsysv,'!SMAP_MAPS','/data/viero/maps/hermes/'
;; (note these three don't all have to be in different places if
;; that's not what you want)
;IRIS MAMD CODE:
defsysv,'!IRISPRO','/home/viero/viero_idl_functions/irispro/'; - should point at the directory where the programs are installed
defsysv,'!IRISDATA','/data/viero/maps/IRIS/'; - should point at the directory where the IRIS data are
defsysv,'!INDEF','-32768'; - should point at the directory where the IRIS data are


;;Next, you need to define the base path to where you put the SVN
;; repository -- this should be the absolute path to the smap_pipeline
;; directory
defsysv,'!SMAP_PIPELINE_PATH','/home/viero/smaproot/smap_pipeline/'
defsysv,'!MY_FUNCTIONS_PATH','/home/viero/viero_idl_functions/'
defsysv,'!ICOSMO_PATH','/data/viero/models/icosmo/'

;;Now we add -all- of the subdirs of this directory to your path
;; If you only want to add specific subdirs, you can replace this line
;; with a set of lines pointing to each individual dir you want.
;;In particular, if you want to use your own astrolib instead of the
;; provided version, you need to change this.
!PATH=EXPAND_PATH('+'+!SMAP_PIPELINE_PATH,/ALL_DIRS)+':'+!PATH
!PATH=EXPAND_PATH('+'+!MY_FUNCTIONS_PATH,/ALL_DIRS)+':'+!PATH
!PATH=EXPAND_PATH('+'+!ICOSMO_PATH,/ALL_DIRS)+':'+!PATH

;;These are some examples of how you could add specific dirs instead
;; of all of them
;!PATH = expand_path('+'+!SMAP_PIPELINE+'astrolib/pro/',/all_dirs)+':'+!PATH
;!PATH = expand_path('+'+!SMAP_PIPELINE+'astrolib/user_contrib/',/all_dirs)+':'+!PATH
;!PATH = expand_path('+'+!SMAP_PIPELINE+'util/',/all_dirs)+':'+!PATH
;!PATH = expand_path('+'+!SMAP_PIPELINE+'powerspec/',/all_dirs)+':'+!PATH

;;Load astrolib
astrolib


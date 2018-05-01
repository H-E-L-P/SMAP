StarFinder v 1.6c



ARCHIVE CONTENT
This archive includes the following files:
+) *.pro:
        IDL source code (tested under IDL v 5.6)
+) astron_for_starfinder.zip:
        compressed archive including the 'astron' routines
        used by StarFinder.
        The 'astron' library may be retrieved from the WEB
        (see the manual for details)
+) *_help.txt:
        ASCII files containing the on-line help pages of the
        IDL widget interface
+) list_of_modules.txt:
        list of IDL source files with a brief description of
        each routine
+) other files (*.fits, *stars.txt):
        simulated field data to be used with the tutorial
        example (see Section 5 of the manual). The files are
        synfield.fits: noisy image of the field
        psf.fits: PSF used to create the field
        background.fits: background added to the field
        noise.fits: array of noise standard deviation for
                    each pixel in the image
        stars.txt: list of positions and fluxes of the stars
                   in the simulated field
        psfstars.txt: list of positions of the stars to be
                      used for PSF estimation


INSTALLATION
Open the compressed archive StarFinder.zip and put all its 
content in a new directory named 'starfinder'. Then modify the 
IDL system variable !Path including the path to the new directory 
(see also the Section 4 of the manual, which can be downloaded 
from the web page).


INFO / BUGS REPORT
Please send information requests and bug reports to:
Emiliano Diolaiti (diolaiti@bo.astro.it)


COPYRIGHT
StarFinder can be used freely.
Please reference the authors in any publication resulting from
the use of the StarFinder code. The most updated reference is:
Diolaiti E., Bendinelli O., Bonaccini D., Close L.M., Currie D.G., 
Parmeggiani G., Astronomy & Astrophysics Supplement Series, 147, 
335, 2000

# ![HELP LOGO](https://avatars1.githubusercontent.com/u/7880370?s=75&v=4) HELP SMAP repository

Documentation on how to instal and use SMAP can be found in the docs folders or [here](https:/herschel.sussex.ac.uk/SMAP)

_____________________________________

This repository contains the SMAP pipeline.  
This svn archive will contain the SMAP code base.

####################

Changelog

Created: April 23, 2009, MZ, described fundamental svn commands.

Mod: May 7, 2009, AC/MZ, A template idl startup file is in the main
 directory called template_idlstart.pro was added, added explanation
 of directory structure to readme.
 
Mod: July 13, 2020, Added HELP logo, put README file in the parent directory, and added some hyperlinks. 

####################

To checkout code from repository:

> cd /home/user/spireroot
> svn checkout svn://smapsvn@spire0.caltech.edu/smap_pipeline

You will need an account on the spire cluster for this to work.


To update local copy with latest version from repository: 
(in smap_pipeline)
> svn update smap_pipeline


To commit local changes to the repository:
(in smap_pipeline)
> svn commit -m "log message"


To get status of project in repository:
(in smap_pipeline)
> svn stat smap_pipeline
or
> svn stat -verbose smap_pipeline


To add new files to the repository:
(in smap_pipeline)
> svn add file_name


To rename files in the repository:
(in smap_pipeline)
> svn rename old_file_name new_file_name


For more info this is a useful little [tutorial](http://artis.imag.fr/~Xavier.Decoret/resources/svn/index.html)

#####################

EXPLANATION OF DIRECTORY STRUCTURE

The architectural philosophy of the archive is to make major
subdirectory divisions along the lines of data products.  Here are the
preliminary divisions, embodied as directories in smap_pipeline:

  - [powerspec](./smap_pipeline/powerspec): programs making power spectra from time streams or taking
power spectra and converting them back to time streams

  - [timestream_analysis](./smap_pipeline/timestream_analysis): programs that function purely on the time series,
for example source extraction, noise estimation, simple filtering, and
so on

  - [mapmaking](./smap_pipeline/mapmaking): a catchall directory for programs that take time series and
convert them into maps

  - [map_analysis](./smap_pipeline/map_analysis): for programs that operate purely on maps, e.g. map point source remove, catalog generator, map correlation functions.

  - [util](./smap_pipeline/util): programs of general use, e.g. reading time series in, making
generic map headers, and so on.

  - [sed](./smap_pipeline/sed): basic SED fitting code (mostly greybody fits)

In addition, the idl astrolib and mpfit libraries have been included
as part of the archive as their functionality is required and my not
be previously installed on user's machines.

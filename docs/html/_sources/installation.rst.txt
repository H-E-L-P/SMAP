Installation
============
SMAP is written in the `IDL <https://www.l3harrisgeospatial.com/Software-Technology/IDL>`_ language and makes use
of the `astrolib <https://idlastro.gsfc.nasa.gov/>`_ and `mpfit <https://www.l3harrisgeospatial.com/docs/mpfit.html>`_
libraries, which are included within this repository.

You will need a working IDL install (with valid licence) to be able to use SMAP. Please refer to
`<https://www.l3harrisgeospatial.com/Software-Technology/IDL>`_ for instructions on how to install IDL and obtain a
licence.

Directory Structure
-------------------
The SMAP repository has the following directory structure (with useful subdirectories shown)

| SMAP
| ├── example_mapmaker.csh (example shell script for running mapmaker)
| ├── HELP_spire_smap_setup.csh (setup shell script, used by example_mapmaker.csh)
| ├── LICENCE.txt
| ├── data
| │   ├── cats
| │   ├── maps
| │   └── timelines
| ├── docs/ (containing html documentation pages)
| ├── source/ (containing raw documentation pages)
| ├── smap_pipeline
| │   └── map_making
| │         └──createmap
| │               └──conffiles (files used by SMAP)
| ├── Makefile (for making documentation)
| ├── smap_idl_startup.pro (idl file for setting directories, used by HELP_spire_smap_setup.csh)
| └── README

In order to make maps, SMAP requires the Herschel SPIRE timelines. They can be found in the
`HELP database <http://hedam.lam.fr/HELP/dataproducts/dmu19/dmu19_timelines/data/reprocessed/>`_ and if running using
the default directory strucure, should be stored in the data/timelines directory.

We have provided an idl startup file, (smap_idl_startup.pro), and a bash shell script (HELP_spire_smap_setup.csh) to
setup relevant directories for SMAP. The following lines may need to be edited if not following the data directory
structure outlined above:

smap_idl_startup.pro
^^^^^^^^^^^^^^^^^^^^
.. code-block:: bash
    :linenos:

    defsysv,'!SMAP_DATA',getenv('GITHUB')+'/data/timelines/'
    ;; Now let's define the path to SPIRE source catalogs
    defsysv,'!SMAP_CATS',getenv('GITHUB_DIR')+'/data/cats/'
    ;; and finally the path to SPIRE maps
    defsysv,'!SMAP_MAPS',getenv('GITHUB_DIR')+'/data/maps/'
    ;; (note these three don't all have to be in different places if
    ;; that's not what you want)

    ;;Next, you need to define the base path to where you put the SVN
    ;; repository -- this should be the absolute path to the smap_pipeline
    ;; directory
    defsysv,'!SMAP_PIPELINE_PATH',getenv('GITHUB_DIR')+'/smap_pipeline/'

HELP_spire_smap_setup.csh
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash
    :linenos:

    # the directory where timelines are stored. Change to where timelines are stored if different
    export DATA_DIR=$GITHUB_DIR/data/timelines
    #the directory where processed maps are saved. Change if required
    export SMAP_DIR=$GITHUB_DIR/data/maps/


.. toctree::
   :maxdepth: 2
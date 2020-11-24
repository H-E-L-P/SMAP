QuickStart
==========
SMAP uses configuration files to produce maps for each field, and example used by HerMES and HELP cabe be found in the
conffiles subdirectory, within the smap_pipeline folder. The configuration files can contain numerous inputs, with the
most common described below:

.. todo:: Describe inputs to conffile

.. note:: you can use the pixsize keyword to change resolution of maps, e.g. to make 1 arcsecond maps you would add pixsize=1 1 1

Before making a map, you will need to download the relevant timelines for the field of
interest (as explained during installation).

Once downloaded, you can use the configuration files to remake the HerMES maps. The example_mapmaker.csh shell script
can be used to launch the mapmaker, by editing conffiles/xmm13hr.conf to your chosen configuration file on the line:

.. code-block:: bash
    :linenos:

    echo 'create_itermap_from_conf, "conffiles/xmm13hr.conf"' > tmp_idl.pro


Once edited, you can simply call the shell script and SMAP should automatically launch and make your chosen map.

.. code-block:: bash
    :linenos:

    source example_mapmaker.csh

.. toctree::
   :maxdepth: 2
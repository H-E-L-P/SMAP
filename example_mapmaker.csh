#!/bin/bash

echo ------------------------
echo Begining script
echo ------------------------

export HOME=~/
cd .

echo about to launch startup script
source ./HELP_spire_smap_setup.csh
echo did it work?
pwd
cd smap_pipeline/map_making/createmap
pwd
echo creating idl script
# In this example we process the map for xmm13hr. Change conffiles/xmm13hr.conf to desired conf file
echo 'create_itermap_from_conf, "conffiles/xmm13hr.conf"' > tmp_idl.pro

echo 'exit' >> tmp_idl.pro
echo which idl?
which idl
idl  tmp_idl.pro
pwd
cd ../../../
pwd
echo ------------------------
echo Ending script
echo ------------------------
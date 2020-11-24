#!/bin/bash
#----------------------------------------------------------------------
# environment variable set up
#----------------------------------------------------------------------

export HOME="~/"

echo setting up the environment for running SMAP code \(on IPAC spire machine\)

export GITHUB_DIR="./"


export IDL_PATH="<IDL_DEFAULT>"
export IDL_PATH=\+$GITHUB_DIR/smap_pipeline/:${IDL_PATH}

export IDL_STARTUP=$GITHUB_DIR/smap_idl_startup.pro

#----------------------------------------------------------------------
# linking some directories and making sure others exist
#----------------------------------------------------------------------


# the directory where timelines are stored. Change to where timelines are stored if different
export DATA_DIR=$GITHUB_DIR/data/timelines
#the directory where processed maps are saved. Change if required
export SMAP_DIR=$GITHUB_DIR/data/maps/

printenv IDL_PATH
printenv IDL_STARTUP
printenv DATA_DIR
printenv GITHUB_DIR

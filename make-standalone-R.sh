#!/usr/bin/env bash

## NOTE: tested on UBUNTU

R_VERSION='2.15.3'

## compile R
if test -f "R-$R_VERSION/_BUILD_SUCCESS" ; then
    echo "R-$R_VERSION has already been built - continuing..."
else
    echo 'download and build R 2.15.3...'
    rm -rf R-$R_VERSION
    curl -o R-$R_VERSION.tar.gz "http://cran.cnr.berkeley.edu/src/base/R-"`echo $R_VERSION | awk -F'.' '{print $1}' `"/R-$R_VERSION.tar.gz" && \
    tar xvzf R-$R_VERSION.tar.gz && \
    cd R-$R_VERSION && \
    ./configure --with-x=no && \
    make && \
    touch _BUILD_SUCCESS &&
    cd ..
fi

if test -f "R-$R_VERSION/_BUILD_SUCCESS" ; then
    rm -f R-$R_VERSION/_PACKAGE_SUCCESS
    echo 'modifying R_HOME_DIR and copying libraries...'
    sed 0,/'R_HOME_DIR=.*'/{s/R_HOME_DIR=.*/'R_HOME_DIR=$(dirname $(readlink -f $0))\/..'/} R-$R_VERSION/bin/R > R.tmp && \
        mv -v R-$R_VERSION/bin/R R-$R_VERSION/bin/R.bak && \
        mv -v R.tmp R-$R_VERSION/bin/R && \
        ls `ldd R-$R_VERSION/bin/exec/R | awk -F'=>' '{print $2}' | awk -F'(' '!($0 ~ /not found/){print $1}' | awk -F'(' '/^[ ]*$/ {next} {print $0}'` | xargs -I {} cp -v {} R-\
$R_VERSION/lib/.`)` && \
        tar -czf R-$R_VERSION-build.tar.gz R-$R_VERSION && \
        touch R-$R_VERSION/_PACKAGE_SUCCESS
else
    echo "ERROR: R-$R_VERSION was not built successfully"
    exit 1
fi

if test -f "R-$R_VERSION/_PACKAGE_SUCCESS" ; then
    echo "R-$R_VERSION was successfully packaged!"
else
    echo "ERROR: R-$R_VERSION was not packaged properly"
    exit 1
fi

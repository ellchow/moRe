#!/usr/bin/env bash

cd `dirname $0`

codePath=`pwd`

importStmt="source('"$codePath"/import.R',chdir=T)"

gr=0

if [ -f ~/.Rprofile ]
then
    gr=`grep "$importStmt" ~/.Rprofile | wc -l`
fi

test $gr -gt 0 && echo ".Rprofile already setup" || (echo $importStmt >> ~/.Rprofile)

#!/bin/bash

spider=/opt/lmod/lmod/libexec/spider

$spider -o softwarePage $1 |\
    iconv -f ISO8859-1 -t UTF8 |\
    sed "s/[\]'/'/g" 
#    python -mjson.tool
#    jsonlint -f

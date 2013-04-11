#!/bin/bash

. /opt/lmod/lmod/init/profile
spider=/opt/lmod/lmod/libexec/spider

for i in $*; do
    $spider -o softwarePage $i |\
        iconv -f ISO8859-1 -t UTF8 |\
        sed "s/[\]'/'/g" 
done | sed ':a;N;$!ba;s/\n//g; s/[]][[]/,/g' | python -mjson.tool

#!/bin/bash

cd /tmp

runspider=/usr/local/bin/runspider.sh
spiderman=/usr/local/bin/spiderman
mf=/opt/modulefiles
json=/tmp/software.json
www=/var/www/software

dirs="$mf/Core $mf/Applications $mf/Development $mf/Libraries $mf/special"

$runspider $dirs > $json
$spiderman -o $www $json
$spiderman -o $www -c application $json
$spiderman -o $www -c development $json
$spiderman -o $www -c library $json
$spiderman -o $www -k chemistry $json
$spiderman -o $www -k physics $json
$spiderman -o $www -k math $json
$spiderman -o $www -k earth $json


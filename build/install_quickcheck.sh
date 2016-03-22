#!/bin/bash

cd /tmp
wget http://www.quviq.com/downloads/eqc-sf-trial2016.zip
unzip eqc-sf-trial2016.zip
cd "Quviq QuickCheck version 1.37.2"
mv /tmp/install_quickcheck.escript .
/usr/local/bin/escript install_quickcheck.escript

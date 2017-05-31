#!/bin/bash

cd /tmp
wget http://quviq-licencer.com/downloads/eqcR19.zip
unzip eqcR19.zip
cd "Quviq QuickCheck version 1.41.1"
mv /tmp/install_quickcheck.escript .
/usr/local/bin/escript install_quickcheck.escript

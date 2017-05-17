#!/bin/bash

cd /tmp
wget http://erlang.org/download/otp_src_19.3.tar.gz
tar -xzvf otp_src_19.3.tar.gz
cd otp_src_19.3/
./otp_build autoconf
./configure
make
make install

cd /tmp
git clone git://github.com/rebar/rebar.git
cd rebar
./bootstrap
mv rebar /usr/local/bin/
chmod +x /usr/local/bin/rebar

rm -rf /tmp/*

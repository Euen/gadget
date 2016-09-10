#!/bin/bash

cd /tmp
wget https://github.com/erlang/otp/archive/OTP-19.0.5.zip
unzip OTP-19.0.5.zip
cd otp-OTP-19.0.5/
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

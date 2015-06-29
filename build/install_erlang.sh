#!/bin/bash

cd /tmp
wget http://erlang.org/download/otp_src_18.1.tar.gz
tar -xvzf otp_src_18.1.tar.gz
cd otp_src_18.1
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
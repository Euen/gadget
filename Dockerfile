FROM debian:8.2

RUN apt-get -y update
RUN apt-get -y dist-upgrade
RUN apt-get install --fix-missing -y build-essential libncurses5-dev openssl libssl-dev fop wget git vim mysql-client runit

COPY build/install_erlang.sh .
RUN ./install_erlang.sh

RUN      mkdir -p /etc/sv/gadget
RUN      mkdir -p /etc/sv/gadget/supervise
RUN      ln -s /etc/sv/gadget /etc/service/gadget
RUN      mkdir /gadget
WORKDIR  /gadget
COPY     . /gadget
RUN      mkdir -p /gadget/dump
COPY     ssh/* .ssh/


RUN make
COPY build/sys.config /gadget/config/app.config
COPY build/run.sh /etc/sv/gadget/run
COPY build/finish.sh /etc/sv/gadget/finish

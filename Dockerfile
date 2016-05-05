FROM ubuntu:latest

RUN apt-get -y update &&\
    apt-get -y dist-upgrade &&\
    apt-get install --fix-missing -y build-essential \
    libncurses5-dev \
    openssl \
    libssl-dev \
    fop \
    wget \
    git \
    vim \
    mysql-client \
    runit \
    libexpat1-dev \
    autoconf \
    default-jdk

COPY build/install_erlang.sh .
RUN ./install_erlang.sh

COPY build/install_quickcheck.sh .
COPY build/install_quickcheck.escript /tmp/
RUN ./install_quickcheck.sh

RUN      mkdir -p /etc/sv/gadget
RUN      mkdir -p /etc/sv/gadget/supervise
RUN      ln -s /etc/sv/gadget /etc/service/gadget
RUN      mkdir /gadget
WORKDIR  /gadget
COPY     . /gadget
RUN      mkdir -p /gadget/dump
COPY     ssh/* /root/.ssh/

RUN make
COPY build/sys.config /gadget/config/app.config
COPY build/run.sh /etc/sv/gadget/run
COPY build/finish.sh /etc/sv/gadget/finish

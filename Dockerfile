FROM java:8

ENV DEBIAN_FRONTEND noninteractive

# INSTALL LEWIS DEPENDENCIES
RUN dpkg --add-architecture i386 && \
    apt-get update && \
    apt-get -y dist-upgrade && \
    apt-get install -yq libstdc++6:i386 zlib1g:i386 libncurses5:i386 --no-install-recommends && \
    apt-get -y install --reinstall locales && \
    dpkg-reconfigure locales && \
    echo 'ja_JP.UTF-8 UTF-8' >> /etc/locale.gen && \
    locale-gen ja_JP.UTF-8 && \
    localedef --list-archive && locale -a &&  \
    update-locale &&  \
    apt-get install --fix-missing -y build-essential \
        libncurses5-dev \
        openssl \
        libssl-dev \
        wget \
        git \
        mysql-client \
        runit \
        vim \
        python-pip \
        libexpat1-dev \
        autoconf \
        openssh-server && \
    apt-get clean

# DOWNLOAD & INSTALL SDK
ENV ANDROID_SDK_URL https://dl.google.com/android/repository/tools_r25.2.3-linux.zip
ENV ANDROID_HOME /usr/local/android-sdk-linux
ENV ANDROID_SDK /usr/local/android-sdk-linux

RUN mkdir -p ${ANDROID_HOME} &&\
    curl -L "${ANDROID_SDK_URL}" > /tmp/android-sdk.zip &&\
    unzip /tmp/android-sdk.zip -d ${ANDROID_HOME} &&\
    rm -rf /tmp/*

ENV PATH ${ANDROID_HOME}/tools:$ANDROID_HOME/platform-tools:$PATH

# SUPPORT GRADLE
ENV TERM dumb
ENV JAVA_OPTS -Xms256m -Xmx512m

RUN echo y | android update sdk --no-ui --all

#INSTALL ERLANG
COPY build/install_erlang.sh .
RUN ./install_erlang.sh

#INSTALL Rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3
RUN mv rebar3 /usr/local/bin
RUN chmod +x /usr/local/bin/rebar3

#INSTALL LEWIS
RUN mkdir workspace
WORKDIR /workspace

RUN git clone https://github.com/inaka/lewis.git
RUN cd lewis && ./gradlew build
RUN cd lewis && ./gradlew install

#SETUP GADGET
RUN      mkdir -p /etc/sv/gadget
RUN      mkdir -p /etc/sv/gadget/supervise
RUN      ln -s /etc/sv/gadget /etc/service/gadget
RUN      mkdir /gadget
WORKDIR  /gadget
COPY     . /gadget
RUN      mkdir -p /gadget/dump
COPY     ssh/* /root/.ssh/


RUN rm -rf _build
RUN rebar3 clean
RUN rebar3 compile
COPY build/sys.config /gadget/config/app.config
COPY build/run.sh /etc/sv/gadget/run
COPY build/finish.sh /etc/sv/gadget/finish

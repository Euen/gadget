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
        fop \
        wget \
        git \
        mysql-client \
        runit \
        vim \
        openjdk-7-jdk \
        python-pip \
        libexpat1-dev \
        autoconf \
        openssh-server && \
    apt-get clean

# DOWNLOAD AND UNTAR SDK
ENV ANDROID_SDK_URL http://dl.google.com/android/android-sdk_r24.3.4-linux.tgz
RUN curl -L "${ANDROID_SDK_URL}" | tar --no-same-owner -xz -C /usr/local
ENV ANDROID_HOME /usr/local/android-sdk-linux
ENV ANDROID_SDK /usr/local/android-sdk-linux
ENV PATH ${ANDROID_HOME}/tools:$ANDROID_HOME/platform-tools:$PATH

# SUPPORT GRADLE
ENV TERM dumb
ENV JAVA_OPTS -Xms256m -Xmx512m
 
COPY build/android_sdk_components.env /android_sdk_components.env
RUN echo y | android update sdk --no-ui --all --filter "$(cat /android_sdk_components.env)"
  
#INSTALL ERLANG
COPY build/install_erlang.sh .
RUN ./install_erlang.sh

COPY build/install_quickcheck.sh .
COPY build/install_quickcheck.escript /tmp/
RUN ./install_quickcheck.sh

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


RUN make
COPY build/sys.config /gadget/config/app.config
COPY build/run.sh /etc/sv/gadget/run
COPY build/finish.sh /etc/sv/gadget/finish

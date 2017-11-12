FROM ubuntu:zesty

# House keeping
RUN apt-get update
RUN apt-get -y upgrade

# Python 3.6 and pip dependencies
RUN apt-get -y install apt-utils python3.6 python3.6-dev python3-pip git gcc
RUN apt-get -y install pkg-config libffi6 autoconf automake libtool openssl libssl-dev

# Solidity
RUN apt-get -y install software-properties-common
RUN add-apt-repository -y ppa:ethereum/ethereum
RUN apt-get update
RUN apt-get -y install solc

# Haskell
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup

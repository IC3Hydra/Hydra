FROM ubuntu:artful

# House keeping
RUN apt-get update 

# Python 3.6 and pip dependencies
RUN apt-get -y install apt-utils python3.6 python3.6-dev python3-pip git gcc && apt-get -y install pkg-config libffi6 autoconf automake libtool openssl libssl-dev

# Solidity
RUN apt-get -y install software-properties-common && add-apt-repository -y ppa:ethereum/ethereum && apt-get update && apt-get -y install solc

# Haskell
RUN apt-get -y install curl && curl -sSL https://get.haskellstack.org/ | sh && stack --resolver lts-9.21 setup

# Prebuild our dependencies
RUN git clone https://github.com/IC3Hydra/Hydra.git && cd Hydra && git reset --hard 82a2a89dd3797f9df94cbd4579e144d68528ed4c && cd hydra/instrumenter && stack test && cd ~ && rm -rf Hydra

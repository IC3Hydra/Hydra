#!/bin/bash

# install script for test virtualenv
# only tested on OS X, but hopefully it also works on linux
# this script assumes that you are running python >= 3.6

set -eo pipefail

if ! [ -e pyethereum ]; then
	git clone https://github.com/ethereum/pyethereum/
fi

if ! [ -e serpent ]; then 
	git clone https://github.com/ethereum/serpent
fi

if ! [ -e viper ]; then
	git clone https://github.com/ethereum/viper
fi

if which brew; then
	brew install pkg-config libffi autoconf automake libtool openssl
fi

virtualenv testenv

source testenv/bin/activate

cd pyethereum
if which brew; then
	env LDFLAGS="-L$(brew --prefix openssl)/lib" CFLAGS="-I$(brew --prefix openssl)/include" pip install -r requirements.txt
else
	pip install -r requirements.txt
fi
python3 setup.py install
cd ..

cd serpent
python3 setup.py install
cd ..

cd viper
python3 setup.py install
cd ..


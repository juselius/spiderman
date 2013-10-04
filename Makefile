all: build

configure: dist/setup-config
	cabal configure

build: configure
	cabal build

install:
	cabal install

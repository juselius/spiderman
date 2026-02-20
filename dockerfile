FROM ubuntu:eoan

# ubuntu 18.04, 16.04, 14.04, 12.04 all have broken `cabal`, "failed to parse .cabal file"
# GHC launchpad provides GHC 7.6.3 for eoan
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan main restricted universe multiverse' > /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-updates main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-security main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-backports main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu eoan main' >> /etc/apt/sources.list
RUN apt-get update
# I tried lots of different GHC versions and all the others had incompatible `base` package
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y git pkg-config ghc-7.6.3 cabal-install-2.4 zlib1g-dev alex happy

RUN ln -s /opt/ghc/7.6.3/bin/ghc /usr/bin/ghc
RUN ln -s /opt/ghc/7.6.3/bin/ghc-pkg /usr/bin/ghc-pkg
RUN ln -s /opt/cabal/bin/cabal /usr/bin/cabal

# unsupported argument --hash-size
# there's probably a better way to tell cabal not to use gold but I don't care
RUN rm /usr/bin/ld.gold && ln -s /usr/bin/ld /usr/bin/ld.gold

# createSymbolicLink: does not exist
# not sure why --global install should make symlink in home dir
# /root/* will be deleted automatically
RUN mkdir -p /root/.cabal/bin

RUN mkdir /opt/spiderman
WORKDIR /opt/spiderman
RUN git clone https://github.com/juselius/spiderman.git .
RUN cabal v1-update
RUN cabal v1-install --global .


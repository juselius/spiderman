FROM ubuntu:eoan

# ubuntu 18.04, 16.04, 14.04, 12.04 all have broken `cabal`, "failed to parse .cabal file"
# GHC launchpad provides GHC 7.6.3 for eoan
RUN sed -i -e 's/archive.ubuntu.com\|security.ubuntu.com/old-releases.ubuntu.com/g' /etc/apt/sources.list
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y haskell-platform git software-properties-common pkg-config

# I tried lots of different GHC versions and all the others had incompatible `base` package
RUN add-apt-repository ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-7.6.3
RUN rm /usr/bin/ghc && ln -s /opt/ghc/7.6.3/bin/ghc /usr/bin/ghc
RUN rm /usr/bin/ghc-pkg && ln -s /opt/ghc/7.6.3/bin/ghc-pkg /usr/bin/ghc-pkg

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
RUN cabal update
# adding these requirements to `spiderman.cabal` didn't work
RUN cabal install --global "texmath==0.6.6.1" "Cabal<3" "pandoc==1.12.4.2" "HStringTemplate>0.3.1" .


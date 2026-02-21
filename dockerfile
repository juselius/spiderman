FROM ubuntu:eoan

# ubuntu 18.04, 16.04, 14.04, 12.04 all have broken `cabal`, "failed to parse .cabal file"
# GHC launchpad provides GHC 7.6.3 for eoan
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan main restricted universe multiverse' > /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-updates main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-security main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb http://old-releases.ubuntu.com/ubuntu/ eoan-backports main restricted universe multiverse' >> /etc/apt/sources.list
RUN echo 'deb [trusted=yes] http://ppa.launchpad.net/hvr/ghc/ubuntu eoan main' >> /etc/apt/sources.list
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
# Adding these requirements to `spiderman.cabal` and `cabal.config` didn't work
RUN cabal v1-install --global \
    # for some reason Cabal 1.20.0.4 and Cabal 2.0.1.1 both get installed
    'Cabal==2.0.1.1' \
    'HStringTemplate==0.7.3' \
    'HTTP==4000.2.23' \
    'aeson==0.7.0.4' \
    'array==0.4.0.1' \
    'asn1-encoding==0.9.5' \
    'asn1-parse==0.9.5' \
    'asn1-types==0.3.4' \
    'async==2.1.1.1' \
    'attoparsec==0.11.3.1' \
    'base==4.6.0.1' \
    'base-orphans==0.9.2' \
    'base64-bytestring==1.0.0.3' \
    'binary==0.5.1.1' \
    'blaze-builder==0.3.3.4' \
    'blaze-html==0.7.1.0' \
    'blaze-markup==0.6.3.0' \
    'byteable==0.1.1' \
    'bytestring==0.10.0.2' \
    'bytestring-builder==0.10.8.2.0' \
    'case-insensitive==1.2.1.0' \
    'cereal==0.5.8.3' \
    'cmdargs==0.10.22' \
    'conduit==1.2.13.1' \
    'connection==0.2.8' \
    'containers==0.5.0.0' \
    'cookie==0.4.3' \
    'cryptonite==0.24' \
    'data-default==0.5.3' \
    'data-default-class==0.1.2.0' \
    'data-default-instances-base==0.1.0.1' \
    'data-default-instances-containers==0.0.1' \
    'data-default-instances-dlist==0.0.1' \
    'data-default-instances-old-locale==0.0.1' \
    'deepseq==1.3.0.1' \
    'deepseq-generics==0.1.1.2' \
    'digest==0.0.1.7' \
    'directory==1.2.0.1' \
    'dlist==1.0' \
    'exceptions==0.10.8' \
    'extensible-exceptions==0.1.1.4' \
    'fail==4.9.0.0' \
    'filepath==1.3.0.1' \
    'foundation==0.0.8' \
    'generic-deriving==1.14.5' \
    'ghc-prim==0.3.0.0' \
    'hashable==1.2.7.0' \
    'highlighting-kate==0.5.15' \
    'hourglass==0.2.12' \
    'hslua==0.3.13' \
    'http-client==0.4.31.2' \
    'http-client-tls==0.2.4.1' \
    'http-conduit==2.1.9' \
    'http-types==0.8.6' \
    'integer-gmp==0.5.0.0' \
    'lifted-base==0.2.3.12' \
    'memory==0.14.8' \
    'mime-types==0.1.0.9' \
    'mmorph==1.2.1' \
    'monad-control==1.0.3.1' \
    'mtl==2.2.2' \
    'nats==1.1.2' \
    'network==2.5.0.0' \
    'old-locale==1.0.0.5' \
    'old-time==1.1.0.1' \
    'pandoc==1.12.4.2' \
    'pandoc-types==1.12.4.7' \
    'parsec==3.1.15.0' \
    'pem==0.2.3' \
    'pretty==1.1.1.0' \
    'primitive==0.6.1.1' \
    'process==1.1.0.2' \
    'random==1.0.1.1' \
    'regex-base==0.93.2' \
    'regex-pcre-builtin==0.94.5.8.8.35' \
    'regex-posix==0.95.2' \
    'resourcet==1.1.3.3' \
    # cabal says it can't find any package by this name
    # but it was included in the output from `cabal freeze`
    # 'rts==1.0' \
    'scientific==0.2.0.2' \
    'semigroups==0.20' \
    'socks==0.5.6' \
    'stm==2.4.5.1' \
    'streaming-commons==0.1.18' \
    'syb==0.4.4' \
    'tagged==0.8.8' \
    'tagsoup==0.13.10' \
    'template-haskell==2.8.0.0' \
    'temporary==1.2.1.1' \
    'texmath==0.6.6.1' \
    'text==1.1.1.4' \
    'th-abstraction==0.7.0.0' \
    'time==1.4.0.1' \
    'tls==1.3.10' \
    'transformers==0.4.3.0' \
    'transformers-base==0.4.6' \
    'transformers-compat==0.7.2' \
    'unix==2.6.0.1' \
    'unordered-containers==0.2.8.0' \
    'utf8-string==0.3.8' \
    'vector==0.10.12.3' \
    'void==0.7.3' \
    'x509==1.6.5' \
    'x509-store==1.6.3' \
    'x509-system==1.6.7' \
    'x509-validation==1.6.8' \
    'xml==1.3.14' \
    'xmlgen==0.6.2.2' \
    'yaml==0.8.27' \
    'zip-archive==0.2.3.7' \
    'zlib==0.5.4.2' \
    .

ENTRYPOINT ["/usr/local/bin/spiderman"]

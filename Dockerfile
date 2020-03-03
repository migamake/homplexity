FROM ubuntu:bionic AS homplexity-build
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update -y
ENV HC=ghc-8.6.5
RUN apt-get install -y cabal-install ${HC} alex happy
ENV HCPKG=ghc-pkg-8.6.5
RUN mkdir -p $HOME/.local/bin
ENV PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/.local/bin:$PATH
RUN mkdir -p /build;
ADD . /build
RUN mkdir -p /build/.git
WORKDIR /build
RUN sed --in-place 's/-- STATIC: //' homplexity.cabal
RUN rm -rf dist-newstyle dist
RUN cabal sandbox init -fstatic
RUN cabal update
RUN cabal install --only-dependencies -fstatic
RUN cabal configure -fstatic
RUN cabal install --bindir=/build/static --libexecdir=/build/static --reinstall -fstatic
RUN ls -alth /build/static

FROM scratch AS homplexity
COPY LICENSE /workdir/LICENSE
COPY --from=homplexity-build /build/static/homplexity-cli /homplexity
WORKDIR /workdir
ENTRYPOINT ["/homplexity"]


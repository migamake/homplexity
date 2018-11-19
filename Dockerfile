FROM ubuntu:bionic AS homplexity-build
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update -y
ENV HC=ghc-8.4.4
RUN apt-get install -y cabal-install ${HC} alex happy
ENV HCPKG=ghc-pkg-8.4.4
RUN mkdir -p $HOME/.local/bin
ENV PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/local/bin:$PATH
RUN mkdir -p /build;
ADD . /build
WORKDIR /build
RUN sed --in-place 's/-- STATIC: //' homplexity.cabal
RUN rm -rf dist-newstyle dist
RUN cabal sandbox init
RUN cabal update
RUN cabal install --only-dependencies
RUN cabal configure
RUN cabal install --bindir=/build/static --libexecdir=/build/static --reinstall
RUN ls -alth /build/static

FROM scratch AS homplexity
COPY --from=homplexity-build /build/static/homplexity-cli /homplexity
ENTRYPOINT ["/homplexity"]


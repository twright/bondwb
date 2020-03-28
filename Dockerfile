FROM ubuntu:18.04 

ARG GHC_VERSION=8.6.5
ARG LTS_SLUG=lts-13.20
ARG PID1_VERSION=0.1.2.0
ARG STACK_VERSION=1.9.3
ARG CUDA_VERSION=10.0
ARG BOOTSTRAP_COMMIT=9f2b7ab95c711794257b059604e80ab9ad3c0c45
ARG DEBIAN_FRONTEND=noninteractive
ARG VARIANT=build

RUN apt-get update && \
    apt-get install -y --no-install-recommends wget netbase ca-certificates gnupg2 && \
    # if [ "$VARIANT" = "small" ]; then \
    echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu bionic main" >>/etc/apt/sources.list && \
    echo "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu bionic main" >>/etc/apt/sources.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 063DAB2BDC0B3F9FCEBC378BFF3AEACEF6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    	ghc-$GHC_VERSION ghc-$GHC_VERSION-htmldocs \
		g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg \
        libtinfo-dev; 
    # else \
        # wget -qO- https://raw.githubusercontent.com/fpco/stackage/$BOOTSTRAP_COMMIT/debian-bootstrap.sh | sed "s/^GHCVER=8.6.3$/GHCVER=$GHC_VERSION/" | bash && \
        # Add g++ version required for building 'double-conversion' \
        # (see https://github.com/commercialhaskell/stack/issues/4470) \
        # apt-get install -y g++-7; \
    # fi && \
    # rm -rf /var/lib/apt/lists/*

#
# Create symlink to help tools find GHC documentation
#

RUN ln -s ghc /opt/ghc/$GHC_VERSION/share/doc/ghc-$GHC_VERSION

#
# Install Stack
#

RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'

#
# Configure Stack to use the GHC installed in the Docker image rather than installing its own
#

RUN mkdir /etc/stack/ && \
    echo "system-ghc: true" >/etc/stack/config.yaml

#
# Use 'stack' to install basic Haskell tools like alex, happy, and cpphs. We
# remove most of the STACK_ROOT afterward to save space, but keep the 'share'
# files that some of these tools require.
#

RUN export STACK_ROOT=/usr/local/lib/stack && \
    stack --resolver=$LTS_SLUG --local-bin-path=/usr/local/bin install \
        cabal-install happy alex cpphs gtk2hs-buildtools hscolour hlint hindent && \
    cd $STACK_ROOT && \
    find . -type f -not -path './snapshots/*/share/*' -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

RUN wget -O- "https://github.com/fpco/pid1/releases/download/v$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1


RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY ./install-deps-ubuntu.sh .
RUN chmod +x ./install-deps-ubuntu.sh
RUN apt-get update && ./install-deps-ubuntu.sh
RUN apt-get update && apt-get install -y vim x11-apps python3 libgtk2.0-dev python3-pip python-pip 2to3

COPY ./requirements.txt .
# RUN pip install -r requirements.txt
RUN pip3 install numpy==1.16.4 && pip3 install -r requirements.txt

RUN 2to3 -w /usr/local/lib/python3.6/dist-packages/stochpy/modules/PyscesMiniModel.py
#--COPY . .
#RUN rm ./stack.yaml && mv ./stack-native.yaml ./stack.yaml
# --RUN stack build && stack install

# Haskell IDE engine
RUN apt-get update && apt-get install -y libicu-dev libtinfo-dev libgmp-dev
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
RUN mkdir -p /root/.local/bin
RUN apt-get update && apt-get install -y dnsutils apt-utils
RUN cd haskell-ide-engine && stack ./install.hs hie-8.6.5
# RUN ln -s /root/.local/bin/hie /usr/local/bin/hie && chmod 755 /root/.local/bin/hie
RUN mkdir /opt/app/src
RUN apt-get update && apt-get install -y zsh
ENV SHELL /bin/zsh
ENV PATH /root/.local/bin:$PATH
# RUN cp /root/.local/bin/hie /usr/local/bin/hie
# RUN cd haskell-ide-engine && stack ./install.hs build-doc

# USER stack
# ENV HOME /home/user
# CMD ["bash"]
ENTRYPOINT ["/usr/local/sbin/pid1"]
FROM haskell:7.8
MAINTAINER bitraten "rph@bitraten.de"

# postgresql-libpq needs pg_config from libpq-dev
RUN apt-get update\
 && apt-get install -y --no-install-recommends libpq-dev\
 && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Add .cabal file
COPY ./bitrest.cabal /opt/bitrest/bitrest.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt/bitrest && cabal install --only-dependencies -j2

# Add and Install Application Code
COPY . /opt/bitrest
RUN cd /opt/bitrest && cabal install

# Add installed cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

EXPOSE 8000

WORKDIR /opt/bitrest
ENTRYPOINT ["./docker-entrypoint.sh"]

# Default Command for Container
CMD ["bitrest"]

FROM ubuntu:focal as BUILDSTAGE
LABEL maintainer="matthew.jadud@gsa.gov"

RUN apt-get update && apt-get install -y apt-transport-https
RUN apt-get install -y build-essential

# By using a version variable, it makes it harder
# to "accidentally" bump the version.
ENV RACKET_VERSION="8.0"
ENV RACKET_SRC_FILE="racket-${RACKET_VERSION}-src.tgz"

WORKDIR /
COPY ${RACKET_SRC_FILE} /${RACKET_SRC_FILE}
RUN tar xvzf ${RACKET_SRC_FILE} && \
    mv racket-${RACKET_VERSION} /racket

WORKDIR /racket/src
RUN ./configure && \
    make && \
    make install 

FROM ubuntu:focal as SETUP
COPY --from=BUILDSTAGE /racket /racket
ENV PATH="/racket/bin:$PATH"

# RUN raco setup --no-docs 
RUN raco pkg config --scope installation --set catalogs \
    https://download.racket-lang.org/releases/8.0/catalog/ \
    https://racksnaps.defn.io/snapshots/2021/03/06/catalog/

RUN raco pkg install --no-docs --skip-installed --auto \
    gregor \
    http-easy \
    sql

WORKDIR /app
# FIXME: this should be a git clone of a tag.
# That would be safer.
COPY . /app 

# Compile the application and package it for distribution.
# This guarantees nothing changes underneath us.
RUN raco exe -o session-counter session-counter.rkt && \
    raco distribute /session-counter session-counter

FROM ubuntu:focal
RUN apt-get install -y tshark && \
    chmod +x /usr/bin/dumpcap
COPY --from=SETUP /session-counter /opt/session-counter
ENV PATH="/opt/session-counter/bin:$PATH"
ENTRYPOINT ["/opt/session-counter"]


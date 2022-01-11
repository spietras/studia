FROM postgis/postgis:14-3.2-alpine

ENV PGROUTING_VERSION 3.3.0
ENV PGROUTING_SHA256 a0953d98a2ce7d7162448bc31f17eba1950ce252f7f292a4953e6291237a16dd

RUN set -eux \
    \
    && apk add --no-cache --virtual .fetch-deps \
        ca-certificates \
        openssl \
        tar \
    \
    && wget -O pgrouting.tar.gz "https://github.com/pgRouting/pgrouting/archive/v${PGROUTING_VERSION}.tar.gz" \
    && echo "$PGROUTING_SHA256 *pgrouting.tar.gz" | sha256sum -c - \
    && mkdir -p /usr/src/pgrouting \
    && tar \
        --extract \
        --file pgrouting.tar.gz \
        --directory /usr/src/pgrouting \
        --strip-components 1 \
    && rm pgrouting.tar.gz \
    \
    && apk add --no-cache --virtual .build-deps \
        cmake \
        make \
        g++ \
        gcc \
        boost-dev \
        perl \
    \
# build pgRouting
    && cd /usr/src/pgrouting \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make \
    && make install \
    \
# clean
    && cd / \
    && rm -rf /usr/src/pgrouting \
    && apk del .fetch-deps .build-deps

WORKDIR /usr/app/postgres/

RUN mkdir -p ./data/ && chown postgres ./data/

USER postgres

COPY --chown=postgres ./src/conf/ ./conf/
COPY --chown=postgres ./src/init/ ./init/
COPY --chown=postgres ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY --chown=postgres ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

FROM postgis/postgis:14-3.2-alpine

ENV PGROUTING_VERSION 3.3.0
ENV PGROUTING_SHA256 a0953d98a2ce7d7162448bc31f17eba1950ce252f7f292a4953e6291237a16dd

# install pgRouting
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
        boost1.76-dev \
        perl \
    \
# build pgRouting
    && cd /usr/src/pgrouting \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make -j$(nproc) \
    && make install \
    \
    && apk add --no-cache --virtual .runtime-deps \
        boost1.76-libs \
    \
# clean
    && cd / \
    && rm -rf /usr/src/pgrouting \
    && apk del .fetch-deps .build-deps

ENV LIBPQXX_VERSION 7.7.0
ENV LIBPQXX_SHA256 2d99de960aa3016915bc69326b369fcee04425e57fbe9dad48dd3fa6203879fb

# install libpqxx
RUN set -eux \
    \
    && apk add --no-cache --virtual .fetch-deps \
        ca-certificates \
        openssl \
        tar \
    \
    && wget -O libpqxx.tar.gz "https://github.com/jtv/libpqxx/archive/${LIBPQXX_VERSION}.tar.gz" \
    && echo "$LIBPQXX_SHA256 *libpqxx.tar.gz" | sha256sum -c - \
    && mkdir -p /usr/src/libpqxx \
    && tar \
        --extract \
        --file libpqxx.tar.gz \
        --directory /usr/src/libpqxx \
        --strip-components 1 \
    && rm libpqxx.tar.gz \
    \
    && apk add --no-cache --virtual .build-deps \
        cmake \
        make \
        g++ \
        gcc \
    \
# build libpqxx
    && cd /usr/src/libpqxx \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make -j$(nproc) \
    && make install \
    \
    && apk add --no-cache --virtual .runtime-deps \
        libpq \
    \
# clean
    && cd / \
    && rm -rf /usr/src/libpqxx \
    && apk del .fetch-deps .build-deps

ENV OSM2PGROUTING_VERSION 2.3.8
ENV OSM2PGROUTING_SHA256 e3a58bcacf0c8811e0dcf3cf3791a4a7cc5ea2a901276133eacf227b30fd8355

# install osm2pgrouting
RUN set -eux \
    \
    && apk add --no-cache --virtual .fetch-deps \
        ca-certificates \
        openssl \
        tar \
    \
    && wget -O osm2pgrouting.tar.gz "https://github.com/pgRouting/osm2pgrouting/archive/v${OSM2PGROUTING_VERSION}.tar.gz" \
    && echo "$OSM2PGROUTING_SHA256 *osm2pgrouting.tar.gz" | sha256sum -c - \
    && mkdir -p /usr/src/osm2pgrouting \
    && tar \
        --extract \
        --file osm2pgrouting.tar.gz \
        --directory /usr/src/osm2pgrouting \
        --strip-components 1 \
    && rm osm2pgrouting.tar.gz \
    \
    && apk add --no-cache --virtual .build-deps \
        cmake \
        make \
        g++ \
        gcc \
        boost1.76-dev \
        expat-dev \
    \
# build osm2pgrouting
    && cd /usr/src/osm2pgrouting \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make -j$(nproc) \
    && make install \
    \
    && apk add --no-cache --virtual .runtime-deps \
        boost1.76-libs \
    \
# clean
    && cd / \
    && rm -rf /usr/src/osm2pgrouting \
    && apk del .fetch-deps .build-deps

ENV OSMCONVERT_SHA256 26d9cd4cc17895b2819a4a7fdfa859c6b5263fa8b346a8f0b375bcc9c8cba90f

# install osmconvert
RUN set -eux \
    && apk add --no-cache --virtual .build-deps musl-dev zlib-dev gcc \
    && wget -O osmconvert.c http://m.m.i24.cc/osmconvert.c \
    && echo "$OSMCONVERT_SHA256 *osmconvert.c" | sha256sum -c - \
    && cc -x c osmconvert.c -lz -O3 -o /usr/local/bin/osmconvert \
    && rm osmconvert.c \
    && apk del .build-deps

WORKDIR /usr/app/postgres/

RUN mkdir -p ./data/ && chown -R postgres ./

USER postgres

COPY --chown=postgres ./src/conf/ ./conf/
COPY --chown=postgres ./src/init/ ./init/
COPY --chown=postgres ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY --chown=postgres ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

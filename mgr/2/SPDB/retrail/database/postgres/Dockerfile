FROM postgres:14.1-alpine3.15

WORKDIR /usr/app/postgres/

RUN mkdir -p ./data/ && chown postgres ./data/

USER postgres

COPY --chown=postgres ./src/conf/ ./conf/
COPY --chown=postgres ./src/init/ ./init/
COPY --chown=postgres ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY --chown=postgres ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

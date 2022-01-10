FROM hoppscotch/hoppscotch:v2.1.0

WORKDIR /usr/app/superset/

COPY ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

FROM hoppscotch/proxyscotch:latest

WORKDIR /usr/app/proxyscotch/

COPY ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

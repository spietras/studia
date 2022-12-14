FROM apache/superset:1.3.2

USER root
RUN pip install --no-cache-dir psycopg2
RUN mkdir -p /usr/app/superset/ && chown superset /usr/app/superset/
USER superset

WORKDIR /usr/app/superset/

COPY --chown=superset ./src/setup.sh ./setup.sh

RUN ./setup.sh

COPY --chown=superset ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

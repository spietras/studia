FROM dpage/pgadmin4:6.3

USER root
RUN mkdir -p /pgadmin4/ /var/lib/pgadmin/ /var/log/pgadmin/ /usr/app/pgadmin/ && \
    chown pgadmin:pgadmin /pgadmin4/ /var/lib/pgadmin/ /var/log/pgadmin/ /usr/app/pgadmin/
USER pgadmin

WORKDIR /usr/app/pgadmin/

COPY --chown=pgadmin:pgadmin ./src/conf/ ./conf/
COPY --chown=pgadmin:pgadmin ./src/setup.sh ./setup.sh

ARG PGADMIN_ADMIN_PASSWORD=admin
ENV PGADMIN_ADMIN_PASSWORD=$PGADMIN_ADMIN_PASSWORD

RUN ./setup.sh -p "$PGADMIN_ADMIN_PASSWORD"

COPY --chown=pgadmin:pgadmin ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

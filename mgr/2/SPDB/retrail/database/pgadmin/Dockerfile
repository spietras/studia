FROM dpage/pgadmin4:6.3

USER root
RUN mkdir -p /pgadmin4/ /var/lib/pgadmin/ /var/log/pgadmin/ && \
    chown pgadmin:pgadmin /pgadmin4/ /var/lib/pgadmin/ /var/log/pgadmin/
USER pgadmin

WORKDIR /usr/app/pgadmin/

COPY --chown=pgadmin:pgadmin ./src/ ./

ENTRYPOINT ["./entrypoint.sh"]

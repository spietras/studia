#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-p PASSWORD] [-h]
    -p, --password                        password to the database for postgres user (default: postgres)
    -h, --help                            prints this message
EOF
}

password="${PGADMIN_POSTGRES_PASSWORD:-postgres}"

while [ "$#" -gt 0 ]; do
  case "$1" in
  -p | --password)
    shift
    password="$1"
    ;;
  -h | --help)
    print_usage
    exit
    ;;
  esac
  shift
done

# save database credentials
echo "postgres:5432:*:postgres:$password" >/tmp/pgpassfile && chmod 600 /tmp/pgpassfile

# restore saved data
mv -n ./data/* /var/lib/pgadmin/

cd /pgadmin4/

# this is copied from original entrypoint
TIMEOUT=$(/venv/bin/python3 -c 'import config; print(config.SESSION_EXPIRATION_TIME * 60 * 60 * 24)')
exec /venv/bin/gunicorn --timeout "${TIMEOUT}" --bind "${PGADMIN_LISTEN_ADDRESS:-[::]}":"${PGADMIN_LISTEN_PORT:-80}" -w 1 --threads "${PGADMIN_GUNICORN_THREADS:-25}" --access-logfile "${PGADMIN_GUNICORN_ACCESS_LOGFILE:--}" -c gunicorn_config.py run_pgadmin:app

#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-u USER] [-p PASSWORD] [-h HOST] [-r PORT] [-d DATABASE] [-a PASSWORD] [--help]
    -u, --user                            user for postgres datasource (default: postgres)
    -p, --password                        password for postgres datasource (default: postgres)
    -h, --host                            host for postgres datasource (default: postgres)
    -r, --port                            port for postgres datasource (default: 5432)
    -d, --database                        database for postgres datasource (default: postgres)
    -a, --admin-password                  password for superset admin user (default: admin)
    --help                                prints this message
EOF
}

user="${SUPERSET_POSTGRES_USER:-postgres}"
password="${SUPERSET_POSTGRES_PASSWORD:-postgres}"
host="${SUPERSET_POSTGRES_HOST:-postgres}"
port="${SUPERSET_POSTGRES_PORT:-5432}"
database="${SUPERSET_POSTGRES_DATABASE:-postgres}"
admin_password="${SUPERSET_ADMIN_PASSWORD:-admin}"

while [ "$#" -gt 0 ]; do
  case "$1" in
  -u | --user)
    shift
    user="$1"
    ;;
  -p | --password)
    shift
    password="$1"
    ;;
  -h | --host)
    shift
    host="$1"
    ;;
  -r | --port)
    shift
    port="$1"
    ;;
  -d | --database)
    shift
    database="$1"
    ;;
  -a | --admin-password)
    shift
    admin_password="$1"
    ;;
  --help)
    print_usage
    exit
    ;;
  esac
  shift
done

# change admin password
superset fab reset-password \
  --username admin \
  --password "$admin_password"

# import database connection
database_uri="postgresql+psycopg2://${user}:${password}@${host}:${port}/${database}"
cat >./datasources.yml <<EOF
databases:
- database_name: $database
  sqlalchemy_uri: $database_uri
EOF
superset import_datasources -p ./datasources.yml
superset set_database_uri -d "$database" -u "$database_uri"

cd /app/
/usr/bin/docker-entrypoint.sh

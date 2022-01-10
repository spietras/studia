#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-p PASSWORD] [-h]
    -p, --password                        password for superset admin user (default: admin)
    -h, --help                            prints this message
EOF
}

password="${SUPERSET_ADMIN_PASSWORD:-admin}"

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

# change admin password
superset fab reset-password \
  --username admin \
  --password "$password"

# import database connection
cat >./datasources.yml <<EOF
databases:
- database_name: retrail
  sqlalchemy_uri: postgresql+psycopg2://postgres:postgres@postgres:5432/retrail
EOF
superset import_datasources -p ./datasources.yml

cd /app/
/usr/bin/docker-entrypoint.sh

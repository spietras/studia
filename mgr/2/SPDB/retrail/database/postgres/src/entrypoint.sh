#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-p PASSWORD] [-a PASSWORD] [-h]
    -p, --password                        password to the database for postgres user (default: postgres)
    -a, --app-password                    password to the database for app user (default: app)
    -h, --help                            prints this message
EOF
}

password="${POSTGRES_ADMIN_PASSWORD:-postgres}"
app_password="${POSTGRES_APP_PASSWORD:-app}"

while [ "$#" -gt 0 ]; do
  case "$1" in
  -p | --password)
    shift
    password="$1"
    ;;
  -a | --app-password)
    shift
    app_password="$1"
    ;;
  -h | --help)
    print_usage
    exit
    ;;
  esac
  shift
done

config_path=$(realpath ./conf/postgresql.conf)
data_path=$(realpath ./data/)

# set data directory to 'data'
export PGDATA="$data_path"

# run postgres in background and save pid
postgres -c config_file="$config_path" &
pid="$!"

# wait until postgres is ready
until pg_isready 2>/dev/null; do
  echo >&2 "Postgres is unavailable - sleeping for 1 second"
  sleep 1
done

# update passwords
# prone to sql injection but why would you try to hurt yourself?
psql -c "ALTER USER postgres WITH PASSWORD '$password';"
psql -c "ALTER USER app WITH PASSWORD '$app_password';"
rm -f ~/.psql_history

# bring postgres to foreground
wait "$pid"

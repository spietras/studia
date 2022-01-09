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

export PGADMIN_DEFAULT_EMAIL=admin@admin.admin # set the default email
export PGADMIN_DISABLE_POSTFIX=yes             # disable mail server

cp -p ./conf/config.py /pgadmin4/config_local.py # copy configuration file
cp -p ./conf/servers.json /pgadmin4/servers.json # copy servers list

echo "postgres:5432:*:postgres:$password" >/tmp/pgpassfile && chmod 600 /tmp/pgpassfile # save database credentials

cd /pgadmin4/

/entrypoint.sh

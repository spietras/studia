#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-h]
    -h, --help                            prints this message
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
  -h | --help)
    print_usage
    exit
    ;;
  esac
  shift
done

config_path=$(realpath ./conf/postgresql.conf)
data_path=$(realpath ./data/)

export PGDATA="$data_path"    # set data directory to 'data'
export POSTGRES_USER=postgres # set the default user to postgres

cp -a ./init/. /docker-entrypoint-initdb.d/ # copy initialization sql files from 'init' directory

docker-entrypoint.sh postgres -c config_file="$config_path"

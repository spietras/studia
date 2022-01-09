#!/bin/bash

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

export PGDATA="$data_path"        # set data directory to 'data'
export POSTGRES_USER=postgres     # set the default user to postgres
export POSTGRES_PASSWORD=postgres # set the default password to postgres

# execute almost everything in entrypoint except running postgres at the end
# this results in the database being fully initialized
. docker-entrypoint.sh
docker_setup_env
docker_create_db_directories
docker_verify_minimum_env
ls /docker-entrypoint-initdb.d/ >/dev/null
docker_init_database_dir
pg_setup_hba_conf postgres -c config_file="$config_path"
export PGPASSWORD="$POSTGRES_PASSWORD"
docker_temp_server_start postgres -c config_file="$config_path"
docker_setup_db
docker_process_init_files ./init/*
docker_temp_server_stop

echo
echo 'PostgreSQL init process complete.'
echo

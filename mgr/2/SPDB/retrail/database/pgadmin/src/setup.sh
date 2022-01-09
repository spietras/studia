#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-p PASSWORD] [-h]
    -p, --password                        password for pgadmin admin user (default: admin)
    -h, --help                            prints this message
EOF
}

password="${PGADMIN_ADMIN_PASSWORD:-admin}"

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

# save current directory
dir="$(pwd)"

cp -p ./conf/config.py /pgadmin4/config_local.py # copy configuration file
cp -p ./conf/servers.json /pgadmin4/servers.json # copy servers list

# set admin account credentials
export PGADMIN_SETUP_EMAIL=admin@admin.admin
export PGADMIN_SETUP_PASSWORD="$password"

cd /pgadmin4/

# this is copied from original entrypoint
# not sure what it does tbh
if [ $(wc -m ./config_distro.py | awk '{ print $1 }') = "0" ]; then
  cat <<EOF >./config_distro.py
CA_FILE = '/etc/ssl/certs/ca-certificates.crt'
LOG_FILE = '/dev/null'
HELP_PATH = '../../docs'
DEFAULT_BINARY_PATHS = {
        'pg': '/usr/local/pgsql-14',
        'pg-14': '/usr/local/pgsql-14',
        'pg-13': '/usr/local/pgsql-13',
        'pg-12': '/usr/local/pgsql-12',
        'pg-11': '/usr/local/pgsql-11',
        'pg-10': '/usr/local/pgsql-10'
}
EOF
  for var in $(env | grep PGADMIN_CONFIG_ | cut -d "=" -f 1); do
    echo ${var#PGADMIN_CONFIG_} = $(eval "echo \$$var") >>./config_distro.py
  done
fi

# setup pgadmin and load servers
# passfiles are loaded at runtime so they are not necessary here
/venv/bin/python3 ./run_pgadmin.py
/venv/bin/python3 ./setup.py --load-servers ./servers.json

# go back to saved working directory
cd "$dir"

# setting up saved the data in /var/lib/pgadmin/
# but this is set as a VOLUME in the base Dockerfile
# so it won't be persistent between RUN commands
# so let's save it somewhere else and later restore it
mkdir ./data/
mv /var/lib/pgadmin/* ./data/

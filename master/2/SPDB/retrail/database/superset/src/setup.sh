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

# create admin user
superset fab create-admin \
  --username admin \
  --firstname Superset \
  --lastname Admin \
  --email admin@superset.com \
  --password admin

# setup internal database
superset db upgrade
superset init

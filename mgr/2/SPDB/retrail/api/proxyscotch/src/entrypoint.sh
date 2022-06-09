#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-h]
    -h, --help                                prints this message
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

cd /etc/proxyscotch
/etc/proxyscotch/out/linux-server/proxyscotch-server-* --host 0.0.0.0:9159

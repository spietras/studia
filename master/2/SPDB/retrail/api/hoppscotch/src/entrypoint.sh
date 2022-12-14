#!/bin/sh

print_usage() {
  # Prints script usage

  cat <<EOF
Usage: $0 [-b URL] [-p URL] [-h]
    -b, --base                                base URL of deployed instance (default: http://localhost:3000)
    -p, --proxy                               proxy URL reachable from user browser
    -h, --help                                prints this message
EOF
}

base="${HOPPSCOTCH_BASE_URL:-http://localhost:3000}"
proxy="$HOPPSCOTCH_PROXY_URL"

while [ "$#" -gt 0 ]; do
  case "$1" in
  -b | --base)
    shift
    base="$1"
    ;;
  -p | --proxy)
    shift
    proxy="$1"
    ;;
  -h | --help)
    print_usage
    exit
    ;;
  esac
  shift
done

export BASE_URL="$base"

cd /app/

# if proxy is passed, change default proxy url
if [ -n "$proxy" ]; then
  find ./packages/hoppscotch-app/ -type f -exec sed -i "s,https://proxy.hoppscotch.io,$proxy,g" {} +
fi

# start the app
pnpm run start

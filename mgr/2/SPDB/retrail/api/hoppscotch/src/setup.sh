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

cd /app/

# disable telemetry and enable proxy
echo "Changing default settings..."
find ./packages/hoppscotch-app/pages/ ./packages/hoppscotch-app/newstore/ -type f -exec sed -i 's,TELEMETRY_ENABLED: true,TELEMETRY_ENABLED: false,g' {} +
find ./packages/hoppscotch-app/pages/ ./packages/hoppscotch-app/newstore/ -type f -exec sed -i 's,EXTENSIONS_ENABLED: true,EXTENSIONS_ENABLED: false,g' {} +
find ./packages/hoppscotch-app/pages/ ./packages/hoppscotch-app/newstore/ -type f -exec sed -i 's,PROXY_ENABLED: false,PROXY_ENABLED: true,g' {} +

# rebuild static site
pnpm run generate

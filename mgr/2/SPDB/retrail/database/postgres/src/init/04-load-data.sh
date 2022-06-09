#!/bin/sh

set -e

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

osm_url='https://download.geofabrik.de/europe/poland/mazowieckie-220101.osm.pbf'
osm_sha='b3fc973643202f30b799bbc773c5f3d73c223590999d412ab9324035210cc5d3'

# download mazovia OSM data in pbf format
wget -O /tmp/mazovia.osm.pbf "$osm_url"
echo "$osm_sha */tmp/mazovia.osm.pbf" | sha256sum -c -

# convert pbf format to raw osm and cut to Warsaw only
osmconvert /tmp/mazovia.osm.pbf -B=./conf/warsaw.poly --drop-author --drop-version --complete-ways --out-osm -o=/tmp/warsaw.osm

# import into the database
osm2pgrouting -f /tmp/warsaw.osm -c ./conf/mapconfig.xml -d retrail -U postgres -h /var/run/postgresql/

# clean up
rm /tmp/mazovia.osm.pbf /tmp/warsaw.osm
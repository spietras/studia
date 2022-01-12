-- Connect to proper database
\c retrail

-- Install postgis extensions
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;

-- Install pgRouting extension
CREATE EXTENSION IF NOT EXISTS pgrouting;

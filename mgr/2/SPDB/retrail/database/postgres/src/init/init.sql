-- Initialization script
-- Runs only once, when there is no database

CREATE DATABASE retrail;

\c retrail

-- Install postgis extensions
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;

-- Install pgRouting extension
CREATE EXTENSION IF NOT EXISTS pgrouting;

-- Create read-only app user
CREATE USER app WITH PASSWORD 'app';
GRANT CONNECT ON DATABASE retrail TO app;
GRANT USAGE ON SCHEMA public TO app;

-- Grant access to all future tables, sequences and functions to app
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON SEQUENCES TO app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT EXECUTE ON FUNCTIONS TO app;

CREATE TABLE test
(
    id   INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL CHECK (name <> '')
);

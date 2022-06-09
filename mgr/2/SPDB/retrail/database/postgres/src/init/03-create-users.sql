-- Connect to proper database
\c retrail

-- Create read-only app user
CREATE USER app WITH PASSWORD 'app';
GRANT CONNECT ON DATABASE retrail TO app;
GRANT USAGE ON SCHEMA public TO app;

-- Grant access to all future tables, sequences and functions to app
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON SEQUENCES TO app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT EXECUTE ON FUNCTIONS TO app;

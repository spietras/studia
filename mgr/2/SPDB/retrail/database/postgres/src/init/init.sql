-- Initialization script
-- Runs only once, when there is no database

CREATE DATABASE retrail;

CREATE TABLE test
(
    id   INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL CHECK (name <> '')
);

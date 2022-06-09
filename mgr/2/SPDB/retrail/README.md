# retrail

Find the best path 🧭

## Quickstart

To run `retrail` locally with all additional services:

```shell
docker-compose up --build
```

To run only core services:

```shell
docker-compose -f docker-compose.yml up --build
```

Then you can go to [`http://localhost:9876`](http://localhost:9876) to view
the `webtrail` web app.

By default the `retrapi` api is available at
[`http://localhost:8765`](http://localhost:8765)
and the `retrail` database is available
at `postgresql://postgres:postgres@localhost:5432/retrail`.

You can change the configuration (like port mappings or passwords) by using
environmental variables. Just take a look at `docker-compose.yml`.

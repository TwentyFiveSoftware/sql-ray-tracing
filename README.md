# Ray Tracing

<img src="https://github.com/TwentyFiveSoftware/ray-tracing-gpu/blob/master/sceneRender.png">

## Overview

This is my take on [Peter Shirley's Ray Tracing in One Weekend](https://github.com/RayTracing/raytracing.github.io)
book.

This project leverages [SQL](https://en.wikipedia.org/wiki/SQL) to perform the ray tracing computations directly within
a database.
The entire ray tracing process is condensed into a single, self-contained SELECT query that doesn't rely on any
pre-existing tables.
Stored procedures and custom functions are not used, and database-specific optimizations are largely avoided to maintain
maximum portability and adhere to standard SQL syntax.
As a result, the query's performance is heavily influenced by the database's query optimizer, JIT compiler, and parallel
query execution capabilities.

## Run this project locally

Settings like width, height, and sample count can be modified in the settings.
They can be found in the first CTE in the `ray-tracing.sql` file.

### Postgres

```shell
docker compose up -d
docker compose exec -T psql-client psql -h postgres -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-postgres.ppm
```

### UmbraDB

Using the Postgres-compatible server:

```shell
docker compose up -d
docker compose exec -T psql-client psql -h umbra -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-umbra.ppm
```

Alternatively, run as a one-off command:

```shell
docker run -it -v ./ray-tracing.sql:/var/db/ray-tracing.sql umbradb/umbra:latest umbra-sql -createdb /var/db/umbra.db /var/db/ray-tracing.sql
```

### ClickHouse

```shell
docker compose up -d
docker compose exec -T clickhouse clickhouse-client --query "CREATE FUNCTION IF NOT EXISTS RANDOM AS () -> randCanonical()"
docker compose exec -T clickhouse clickhouse-client --time --queries-file /opt/ray-tracing.sql > render-clickhouse.ppm
```

## Performance

The performance was measured on the same scene (see image above) with the same number of objects, the same recursive
depth, the same resolution (800 x 450). The measured times are averaged over multiple runs.

|            | 1 sample / pixel | 100 samples / pixel | 
|------------|-----------------:|--------------------:|
| ClickHouse |          199.1 s |                 N/A |
| Postgres   |           53.7 s |                 N/A |
| Umbra      |            1.2 s |                 N/A |

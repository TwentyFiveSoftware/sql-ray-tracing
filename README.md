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

### PostgreSQL

```shell
docker compose up -d
docker compose exec -T psql-client psql -h postgres -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-postgres.ppm
```

### UmbraDB (very fast)

Using the Postgres-compatible server:

```shell
docker compose up -d
docker compose exec -T psql-client psql -h umbra -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-umbra.ppm
```

Alternatively, run as a one-off command:

```shell
docker run -it -v ./ray-tracing.sql:/var/db/ray-tracing.sql umbradb/umbra:latest umbra-sql -createdb /var/db/umbra.db /var/db/ray-tracing.sql
```

### ClickHouse (very slow)

```shell
docker compose up -d
docker compose exec -T clickhouse clickhouse-client --query "CREATE FUNCTION IF NOT EXISTS RANDOM AS () -> randCanonical()"
docker compose exec -T clickhouse clickhouse-client --max_subquery_depth=100000000 --max_recursive_cte_evaluation_depth=1000000 --queries-file /opt/ray-tracing.sql > render-clickhouse.ppm
```

## Performance

|          | 1 sample / pixel | 100 samples / pixel | 
|----------|-----------------:|--------------------:|
| Postgres |        64,000 ms |                 N/A |
| Umbra    |         1,500 ms |                 N/A |

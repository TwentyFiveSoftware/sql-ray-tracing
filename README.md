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
docker compose exec -it postgres psql -U postgres -c '\timing' -f /opt/ray-tracing.sql
```

### UmbraDB (very fast)

```shell
docker run -it -v ./ray-tracing.sql:/var/db/ray-tracing.sql umbradb/umbra:latest umbra-sql -createdb /var/db/umbra.db /var/db/ray-tracing.sql
```

### ClickHouse (very slow)

```shell
docker compose up -d
docker compose exec -it clickhouse clickhouse-client --query "SET max_subquery_depth = 100000000"
docker compose exec -it clickhouse clickhouse-client --queries-file /opt/ray-tracing.sql
```

Required modifications to `ray-tracing.sql`:

- Put `SET max_subquery_depth = 100000;` before SELECT the query
- Uncomment lines annotated with `-- ClickHouse`
- Replace all `RANDOM()` with `RAND()`

## Performance

Not benchmarked yet.


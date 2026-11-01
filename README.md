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

### CedarDB

```shell
docker compose up -d
docker compose exec -T -e PGPASSWORD=super-secret-password psql-client psql -h cedardb -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-cedardb.ppm
```

### UmbraDB

```shell
docker compose up -d
docker compose exec -T psql-client psql -h umbra -t -A -o /dev/stderr -c '\timing on' -c '\i /opt/ray-tracing.sql' 2> render-umbra.ppm
```

### ClickHouse

```shell
docker compose up -d
docker compose exec -T clickhouse clickhouse-client --query "CREATE FUNCTION IF NOT EXISTS RANDOM AS () -> randCanonical()"
docker compose exec -T clickhouse clickhouse-client --time --queries-file /opt/ray-tracing.sql > render-clickhouse.ppm
```

## Performance

The performance was measured on the same scene (see image above) with the same number of objects, the same recursive
depth, the same resolution. The measured times are averaged over multiple runs.

### Resolution: 1920 x 1080

|         | 1 sample / pixel | 10 samples / pixel | 
|---------|-----------------:|-------------------:|
| Umbra   |            5.8 s |             55.6 s |
| CedarDB |            1.2 s |             10.9 s |

### Resolution: 800 x 450

|            | 1 sample / pixel | 10 samples / pixel |
|------------|-----------------:|-------------------:|
| ClickHouse |          199.1 s |                N/A |
| Postgres   |           53.7 s |                N/A |
| Umbra      |            1.2 s |             10.1 s |
| CedarDB    |            0.3 s |              2.9 s |

*Note: While CedarDB is by far the fastest, the resulting rendered output contains visual errors and is
currently incorrect.*

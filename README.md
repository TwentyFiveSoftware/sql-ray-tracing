## PostgreSQL
```shell
docker compose up -d
docker compose exec -it postgres psql -U postgres -c '\timing' -f /opt/ray-tracing.sql
```

## UmbraDB
```shell
docker run -it -v ./ray-tracing.sql:/var/db/ray-tracing.sql umbradb/umbra:latest umbra-sql -createdb /var/db/umbra.db /var/db/ray-tracing.sql
```

## ClickHouse
```shell
docker compose up -d
docker compose exec -it clickhouse clickhouse-client --query "SET max_subquery_depth = 100000000"
docker compose exec -it clickhouse clickhouse-client --queries-file /opt/ray-tracing.sql
```

Required modifications to `ray-tracing.sql`:
- Uncomment lines annotated with `-- ClickHouse`
- Put `SET max_subquery_depth = 100000;` before the query
- Replace all `RANDOM()` with `RAND()`

#!/bin/bash

export DATABASE_URL="host=${POSTGRES_PORT_5432_TCP_ADDR} port=${POSTGRES_PORT_5432_TCP_PORT} user=postgres password=${POSTGRES_ENV_POSTGRES_PASSWORD}"

if [ $# -gt 0 ]; then
    exec "$@"
fi

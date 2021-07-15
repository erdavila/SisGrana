#!/bin/bash

read -p 'Tem certeza?! ' CONFIRMATION
if [ "$CONFIRMATION" != "SIM!" ]; then
  exit
fi

set -ex
rm data/investments.sqlite
sqlite3 data/investments.sqlite < sql/schema.sql
scripts/import.sh data/2019/* data/2020/* data/2021/*
scripts/import-quotes.sh data/teste.zip quotes/COTAHIST_A2021.ZIP quotes/COTAHIST_D28052021.ZIP scripts/recreate-database.sh
scripts/assets-at-date.sh

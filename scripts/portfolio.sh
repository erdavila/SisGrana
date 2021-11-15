#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.portfolio.PortfolioMain
exec bash $(dirname $0)/_executor.sh "$@"

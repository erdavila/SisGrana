#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.funds.FundsMain
exec bash $(dirname $0)/_executor.sh "$@"

#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.incomeRate.IncomeRateMain
exec bash $(dirname $0)/_executor.sh "$@"

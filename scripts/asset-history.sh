#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.assetHistory.AssetHistoryMain
exec bash $(dirname $0)/_executor.sh "$@"

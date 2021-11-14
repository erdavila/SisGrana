#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.assetsAtDate.AssetsAtDateMain
exec bash $(dirname $0)/_executor.sh "$@"

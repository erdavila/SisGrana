#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.multiImport.MultiImportMain
exec bash $(dirname $0)/_executor.sh "$@"

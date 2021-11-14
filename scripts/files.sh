#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.files.FilesMain
exec bash $(dirname $0)/_executor.sh "$@"

#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.processQuotes.ProcessQuotesMain
exec bash $(dirname $0)/_executor.sh "$@"

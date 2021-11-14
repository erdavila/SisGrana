#!/bin/bash
export MAIN_CLASS=sisgrana.investments.commands.irpf.IrpfMain
exec bash $(dirname $0)/_executor.sh "$@"

#!/bin/bash

ARGS=''
for arg in "$@"; do
  ARGS="$ARGS \"$arg\""
done

sbt "runMain sisgrana.investments.variableIncome.processQuotes.ProcessQuotesMain $ARGS"
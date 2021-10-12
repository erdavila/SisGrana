#!/bin/bash

FILES=''
for file in "$@"; do
  FILES="$FILES \"$file\""
done

sbt "runMain sisgrana.investments.variableIncome.importQuotes.ImportQuotesMain $FILES"

#!/bin/bash
set -e
stack build

stack exec ingcsv2hledger-exe *.csv | tee ingcsvs.journal

hledger -f ingcsvs.journal accounts

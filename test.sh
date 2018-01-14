#!/bin/bash

stack exec ingcsv2hledger-exe | tee a.journal

hledger -f a.journal accounts

#!/usr/bin/env bash

set -eu
set -o pipefail

abstract="$(./opt -I "$1" | sed 's/, /\n/g')"
concrete="$(./compile_and_run -O2 "$1")"
#paste <(echo "$abstract") <(echo "$concrete")

awk '
{
    if (NR == FNR) {
        # First file: abstract values
        if (index($2, "I_")) {
            # I_n is the nth input (assumed to be n)
            values[$1] = substr($2, 3)
        } else {
            values[$1] = $2
        }
    } else {
        # Second file: concrete values
        if (!index(values[$1], "?_") && values[$1] != $2) {
            printf "%s has value %s, but abstract value was %s\n", $1, $2, values[$1]
            exit 1
        } else {
            # ?_n can be any value, so ?_n >= x, for all x
            printf "%s %s <= %s\n", $1, $2, values[$1]
        }
    }
}
' <(echo "$abstract") <(echo "$concrete")

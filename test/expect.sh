#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

OUTPUT=test/"$*"
OUTPUT="${OUTPUT// /}"
> "$OUTPUT.output"

for test in test/*.txt; do
    { echo "$(basename "$test")"; ./$* "$test"; echo; } >> "$OUTPUT.output"
done

if git diff --no-index "$OUTPUT.expected" "$OUTPUT.output"; then
    rm "$OUTPUT.output"
else
    echo
    read -rp "Accept this output [y/n]? " -n1
    echo
    if [[ $REPLY =~ [Yy] ]]; then
        mv "$OUTPUT.output" "$OUTPUT.expected"
    elif [[ $REPLY =~ [Nn] ]]; then
        exit 1
    fi
fi

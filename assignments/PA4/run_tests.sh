#!/bin/bash

errors=false;
for file in tests/*.cl; do
    if [ -f ${file} ]; then
	output=$(./sdiff $file)
	if [ -n "$output" ]; then
	    echo "Diff in file ${file}:\n'${output}'";
	    errors=true;
	fi;
    fi;
done;
if [ $errors = false ]; then
  printf "\n\nAll tests pass!\n";
fi


#!/bin/bash

errors=false;
for file in ./tests/*.cl ../PA2/tests/*.cl; do
     if [ -f ${file} ]; then
         output=$(./pdiff $file)
         if [ -n "$output" ]; then
             echo "Diff in file ${file}:\n'${output}\n\n'";
             errors=true;
         fi;
     fi;
done;
if [ $errors = false ]; then
  printf "\n\nAll tests pass!\n";
fi


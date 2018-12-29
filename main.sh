#!/usr/bin/env bash
set -o errexit

if [[ ! $1 ]] || [[ $# -ge 2 ]]; then
  echo "Usage: ./main.sh code.rat" 2>&1
  exit
fi

if [[ ! -e $1 ]]; then
  echo "Error: No such file or directory" 2>&1
  exit
fi

dune exec ./main.exe $1 > _main.tam
java -jar runtam.jar _main.tam
rm _main.tam

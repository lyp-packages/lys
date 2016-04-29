#!/usr/bin/env bash

# Compile command line arguments into a scheme expression to be passed to the 
# server. The first item in the list is the client's working directory, followed
# by the client's name (so getopt-long will be happy), and then the actual
# arguments.
opts="(lys:compile-file \"$(pwd)\""
for v in "$@"
do
  # escape double quotes
  opts="$opts \"$(sed 's/"/\\"/g' <<< $v)\""
done
opts="$opts)"

# echo $opts

echo -n "$opts" | nc localhost 1225
if (($? == 1)); then
  echo "Could not find lilypond server on port 1225"
  exit 1
fi
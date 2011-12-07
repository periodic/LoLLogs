#!/bin/bash

url=$1
file="champions.dat"


params=`awk -F"	" '{ printf("f2=%s&f3=%s&f4=%s\n", $1, $2, $3) }' $file`

IFS=$'\n'
set -f
for line in $params
do
    nonce=`curl --cookie-jar /tmp/cookies http://localhost:3000/champion/create 2>/dev/null | perl -ne '/_nonce" value="(\w+)"/; print $1;'`
    curl -X POST --cookie /tmp/cookies --data "$line&_nonce=$nonce" $url
done
set +f
unset IFS

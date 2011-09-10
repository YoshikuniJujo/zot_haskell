#!/bin/sh

cat $1 | ./lambdaToSKI.sh | ./skiToZot.sh | ./addEcho.sh $2 | ./zot.sh

#!/bin/sh

cat $1 | ./zot.sh lambdaToSki | ./zot.sh skiToZot | ./zot.sh arg $2 | ./zot.sh -

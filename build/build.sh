#! /bin/sh

SCRIPT=`readlink -f $0`
DIR=`dirname $SCRIPT`

buildapp \
    --asdf-path ~/asd \
    --load-system cl-uglify-js \
    --load-system unix-options \
    --load "$DIR/main.lisp" \
    --entry cl-uglify-js.main::main \
    --output cl-uglify-js

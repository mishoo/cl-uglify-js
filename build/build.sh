#! /bin/sh

SCRIPT=`readlink -f $0`
DIR=`dirname $SCRIPT`

buildapp \
    --eval "(proclaim '(optimize (speed 3) (safety 0) (debug 0)))" \
    --asdf-path ~/lisp/systems \
    --asdf-tree ~/quicklisp \
    --asdf-path ~/asd \
    --load-system cl-uglify-js \
    --load-system unix-options \
    --load "$DIR/main.lisp" \
    --entry cl-uglify-js.main::main \
    --output cl-uglify-js

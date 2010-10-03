(cl:defpackage #:cl-uglify-js
  (:use #:cl #:parse-js)
  (:import-from #:parse-js #:defun/defs #:with-defs)
  (:nicknames #:uglify-js))

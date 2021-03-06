(asdf:defsystem #:cl-uglify-js
  :description "JavaScript compressor/beautifier on top of PARSE-JS"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :depends-on (#:parse-js #:cl-ppcre #:cl-ppcre-unicode #:parse-number #:iterate)
  :components
  ((:module "src" :serial t
            :components ((:file "package")
                         (:file "constants")
                         (:file "walker")
                         (:file "mangle")
                         (:file "squeeze")
                         (:file "codegen")
                         (:file "split")))))

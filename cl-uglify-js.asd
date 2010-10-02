(asdf:defsystem #:cl-uglify-js
  :description "JavaScript compressor/beautifier on top of PARSE-JS"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :depends-on (#:parse-js #:cl-ppcre)
  :components
  ((:module "src" :serial t
            :components ((:file "package")
                         (:file "constants")
                         (:file "codegen")
                         (:file "uglify-js")))))
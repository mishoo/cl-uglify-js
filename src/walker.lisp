(in-package #:uglify-js)

(defmacro ast-walk ((ast &key (walk 'walk) (expr 'expr)) &body body)
  `(labels ((,walk (,expr)
              (if (eq ,expr t)
                  t
                  (etypecase ,expr
                    (string ,expr)
                    (integer ,expr)
                    (keyword ,expr)
                    (list (or (progn ,@body)
                              (mapcar #',walk ,expr)))))))
     (,walk ,ast)))

(in-package #:uglify-js)

(defmacro ast-case (expr &body body)
  (let ((ex (gensym "AST")))
    `(let ((,ex ,expr))
       (case (car ,ex)
         ,@(loop :for (c a b) :in body
              :if a :collect `(,c (destructuring-bind ,a (cdr ,ex) ,b))
              :else :collect `(,c ,b))))))

(defmacro ast-walk ((ast &key (walk 'walk) (expr 'expr)) &body body)
  `(labels ((,walk (,expr)
              (if (eq ,expr t)
                  t
                  (if (eq ,expr nil)
                      nil
                      (etypecase ,expr
                        (string ,expr)
                        (integer ,expr)
                        (keyword ,expr)
                        (list (or (progn ,@body)
                                  (mapcar #',walk ,expr))))))))
     (,walk ,ast)))
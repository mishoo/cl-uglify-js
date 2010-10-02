(in-package #:uglify-js)

;;; ast-to-code generator

(defmacro stick (&rest elements)
  `(apply #'concatenate 'string (remove-if #'null (list ,@elements))))

(defun repeat-string (str n)
  (declare (type (integer 0) n))
  (cond
    ((= n 0) "")
    ((= n 1) str)
    (t (let ((dbl (repeat-string str (ash n -1))))
         (setq dbl (concatenate 'string dbl dbl))
         (if (oddp n)
             (concatenate 'string dbl str)
             dbl)))))

;;; <cl-json> --- the following is taken from cl-json
(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
       do (write-char ch stream)
     else
       do (let ((special '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
            (destructuring-bind (esc . (width . radix)) special
              (format stream "\\~C~V,V,'0R" esc radix width code)))))
;;; </cl-json>

(defun quote-string (str)
  (with-output-to-string (out)
    (write-char #\" out)
    (write-json-chars str out)
    (write-char #\" out)))

(defmacro ast-case (expr &body body)
  (let ((ex (gensym)))
    `(let ((,ex ,expr))
       (case (car ,ex)
         ,@(loop :for (c a b) :in body
              :if a :collect `(,c (destructuring-bind ,a (cdr ,ex) ,b))
              :else :collect `(,c ,b)
              :end)))))

(defun ast-gen-code (ast &key
                     (beautify *codegen-beautify*)
                     (indent-level *codegen-indent-level*)
                     (indent-start *codegen-indent-start*)
                     (quote-keys *codegen-quote-keys*))
  (let ((indentation 0))
    (macrolet ((with-indent (inc &body body)
                 `(prog2
                      (incf indentation ,inc)
                      (progn ,@body)
                    (decf indentation ,inc))))
      (labels ((indent (str)
                 (if beautify
                     (stick (repeat-string " " (+ (* indentation indent-level) indent-start)) str)
                     str))

               (join (list sep1 &optional sep2)
                 (with-output-to-string (out)
                   (loop :for cons :on list
                      :do (write-string (car cons) out)
                      :when (cdr cons) :do (if beautify
                                               (write-string sep1 out)
                                               (when sep2
                                                 (write-string sep2 out))))))

               (format-body (body &optional (nl *codegen-newline*))
                 (if body
                     (join `("{"
                             ,@(with-indent 1 (mapcar #'indent (mapcar #'walk body)))
                             ,(indent "}"))
                           nl)
                     "{}"))

               (add-spaces (&rest elements)
                 (setf elements (delete-if #'null elements))
                 (cond
                   (beautify (format nil "~{~A~^ ~}" elements))
                   (t (apply #'concatenate 'string (loop :for (i next) :on elements
                                                      :when i :collect i
                                                      :when (and next
                                                                 (or
                                                                  (and (ppcre:scan "(?i)[a-z0-9_$]$" i)
                                                                       (ppcre:scan "^(?i)[a-z0-9_$]" next))
                                                                  (and (ppcre:scan "[+-]$" i)
                                                                       (ppcre:scan "^[+-]" next)))) :collect " ")))))

               (parenthesize (expr &rest cases)
                 (let ((str (walk expr)))
                   (loop :for i :in cases :do
                      (typecase i
                        (function (when (funcall i expr) (return (stick "(" str ")"))))
                        (t (when (eq (car expr) i) (return (stick "(" str ")")))))
                      :finally (return str))))

               (make-then (th)
                 (let ((b th))
                   (loop :for type = (first b) :do
                      (case type
                        (:if (when (not (fourth b))
                               ;; no else for this IF, we need the block
                               (return (walk `(:block (list ,th))))))
                        ((:while :do) (setf b (third b)))
                        ((:for :for-in) (setf b (fifth b)))
                        (t (return (walk th)))))))

               (walk (ast)
                 ;; someone tell me how do I trick Emacs to indent this properly.
                 (ast-case ast

                   (:toplevel (body) (join (mapcar #'indent (mapcar #'walk body)) *codegen-tl-newline*))

                   (:block (body) (format-body body))

                   ((:var :const) (defs)
                    (stick (ecase (car ast)
                             (:var "var ")
                             (:const "const "))
                           (join (mapcar (lambda (def)
                                           (if (cdr def)
                                               (add-spaces (car def) "=" (walk (cdr def)))
                                               (car def))) defs)
                                 ", " ",")
                           ";"))

                   (:return (expr) (stick (add-spaces "return" (walk expr)) ";"))

                   (:throw (expr) (stick (add-spaces "throw" (walk expr)) ";"))

                   ((:function :defun) (name args body)
                    (add-spaces (stick (add-spaces "function" name) "(" (join args ", " ",") ")")
                                (format-body body)))

                   (:stat (stmt)
                          (stick (walk stmt) ";"))

                   (:array (a)
                           (if (not a) "[]"
                               (add-spaces "[" (join (mapcar #'walk a) ", " ",") "]")))

                   (:object (props)
                            (if (not props) "{}"
                                (join `("{"
                                        ,(join (with-indent 1
                                                 (mapcar (lambda (p)
                                                           (indent (join `(,(if (or quote-keys (not (is-identifier (car p))))
                                                                                (quote-string (car p))
                                                                                (car p))
                                                                            ,(walk (cdr p)))
                                                                         " : " ":"))) props))
                                               #.(format nil ",~%") ",")
                                        ,(indent "}")) *codegen-newline*)))

                   (:binary (op left right)
                            (let ((lvalue (walk left))
                                  (rvalue (walk right)))
                              (when (or (member (car left) '(:assign :conditional :seq))
                                        (and (eq (car left) :binary)
                                             (> (precedence op) (precedence (cadr left)))))
                                (setf lvalue (format nil "(~A)" lvalue)))
                              (when (or (member (car right) '(:assign :conditional :seq))
                                        (and (eq (car right) :binary)
                                             (>= (precedence op) (precedence (cadr right)))))
                                (setf rvalue (format nil "(~A)" rvalue)))
                              (add-spaces lvalue (operator-string op) rvalue)))

                   (:unary-prefix (op expr)
                                  (with-output-to-string (out)
                                    (setf op (operator-string op))
                                    (write-string op out)
                                    (when (char>= (char op 0) #\A)
                                      (write-char #\Space))
                                    (write-string (parenthesize expr :num #'dot-call-parens) out)))

                   (:unary-postfix (op expr)
                                   (stick (parenthesize expr :num #'dot-call-parens)
                                          (operator-string op)))

                   (:num (n) (format nil "~A" n))

                   (:string (str) (quote-string str))

                   (:name (name) name)

                   (:atom (a) (string-downcase (string a)))

                   (:regexp (pattern modifiers)
                            (stick "/" pattern "/" modifiers))

                   (:try (tr ca fi)
                         (add-spaces "try" (walk tr)
                                     (when ca (add-spaces "catch" (stick "(" (car ca) ")") (walk (cdr ca))))
                                     (when fi (add-spaces "finally" (walk fi)))))

                   (:assign (op left right)
                            (add-spaces (walk left)
                                        (if (eq op t)
                                            "="
                                            (stick (operator-string op) "="))
                                        (walk right)))

                   (:new (ctor args)
                         (with-output-to-string (out)
                           (write-string
                            (add-spaces "new" (parenthesize ctor :seq :binary :conditional :assign
                                                            (lambda (expr)
                                                              (labels ((walk (ex)
                                                                         (ast-case ex
                                                                           (:call () (throw 'has-call t)))))
                                                                (catch 'has-call
                                                                  (walk expr)))))) out)
                           (when args
                             (write-string (stick "(" (join (mapcar #'walk args) ", " ",") ")") out))))

                   (:break (label) (add-spaces "break" label))

                   (:continue (label) (add-spaces "continue" label))

                   (:conditional (cond then else)
                                 (add-spaces (parenthesize cond :assign :seq)
                                             "?"
                                             (parenthesize then :seq)
                                             ":"
                                             (parenthesize else :seq)))

                   (:call (expr args)
                          (stick (parenthesize expr #'dot-call-parens)
                                 "(" (join (mapcar #'walk args) ", " ",") ")"))

                   (:dot (expr prop)
                         (stick (parenthesize expr #'dot-call-parens) "." prop))

                   (:if (cond then else)
                        (apply #'add-spaces `("if" ,(stick "(" (walk cond) ")")
                                                   ,(if else (make-then then) (walk then))
                                                   ,@(if else `("else" ,(walk else))))))

                   (:for (init cond step body)
                         (let ((args (join (list (if init (ppcre:regex-replace ";+$" (walk init) "") "")
                                                 (if cond (ppcre:regex-replace ";+$" (walk cond) "") "")
                                                 (if step (ppcre:regex-replace ";+$" (walk step) "") ""))
                                           "; " ";")))
                           (when (string= args "; ; ") (setf args ";;"))
                           (add-spaces "for" (stick "(" args ")")
                                       (walk body))))

                   (:for-in (has-var key hash body)
                            (with-output-to-string (out)
                              (write-string (add-spaces "for" "(") out)
                              (when has-var (write-string "var " out))
                              (write-string (add-spaces (stick key " in " (walk hash) ")")
                                                        (walk body)) out)))

                   (:while (cond body)
                           (add-spaces "while" (stick "(" (walk cond) ")")
                                       (walk body)))

                   (:with (expr body)
                          (add-spaces "with" (stick "(" (walk expr) ")")
                                      (walk body)))

                   (:do (cond body)
                        (add-spaces "do" (walk body) "while" (stick "(" (walk cond) ");")))

                   (:sub (expr sub)
                         (stick (parenthesize expr #'dot-call-parens) "[" (walk sub) "]"))

                   (:seq (one two)
                         (join (list (walk one) (walk two)) ", " ","))

                   (:label (label body)
                           (add-spaces (stick label ":") (walk body)))

                   (:case (expr) (stick (add-spaces "case" (walk expr)) ":"))

                   (:default () "default:")

                   (:switch (expr body)
                            (add-spaces "switch" (stick "(" (walk expr) ")")
                                        (format-body body)))

                   ;; (t () (error "Can't handle ~A" ast))
                   )))
        (walk ast)))))

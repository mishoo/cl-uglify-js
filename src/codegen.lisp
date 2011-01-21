(in-package #:uglify-js)

;;; ast-to-code generator

(defun stick (&rest elements)
  (declare (inline stick))
  (apply #'concatenate 'string (delete-if #'null elements)))

;;; <cl-json> --- the following is taken from cl-json
(defun write-json-chars (quote s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (write-char quote stream)
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (eq ch quote)
       do (write-char #\\ stream) (write-char ch stream)
     else if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else do (write-char ch stream))
  (write-char quote stream))
;;; </cl-json>

(defun write-regexp (s stream)
  (declare (inline write-regexp))
  (loop :for ch :across s
     :for code = (char-code ch)
     :if (< #x1f code #x7f) :do (write-char ch stream)
     :else :if (< code #x100) :do (format stream "\\x~16,2,'0R" code)
     :else :do (format stream "\\u~16,4,'0R" code)))

(defun quote-string (str)
  (declare (inline quote-string))
  (with-output-to-string (out)
    (write-json-chars (if (<= (count #\" str) (count #\' str))
                          #\"
                          #\') str out)))

(defun ast-gen-code (ast &key
                     (beautify *codegen-beautify*)
                     (indent-level *codegen-indent-level*)
                     (indent-start *codegen-indent-start*)
                     (quote-keys *codegen-quote-keys*))
  (let ((indentation 0)
        (stack nil))
    (macrolet ((with-indent (inc &body body)
                 `(prog2
                      (incf indentation ,inc)
                      (progn ,@body)
                    (decf indentation ,inc))))
      (labels ((indent (str)
                 (declare (inline indent))
                 (if beautify
                     (concatenate 'string (make-string (round (+ (* indentation indent-level) indent-start)) :initial-element #\Space) str)
                     str))

               (precedence (op)
                 (declare (ftype (function (keyword) (integer 0 100))))
                 (declare (inline precedence))
                 (gethash op *precedence*))

               (operator-string (op)
                 (declare (inline operator-string))
                 (string-downcase (string op)))

               (needs-parens (expr)
                 (case (car expr)
                   (:function           ; dot/call on a function
                                        ; requires the parens only
                                        ; when the function identifier
                                        ; appears as the first "thing"
                                        ; in a statement.
                    (loop
                       :for (this p) :on stack
                       :when (eq (car p) :stat) :do (return t)
                       :unless (and (eq (car p) :seq)
                                    (eq (cadr p) this)) :do (return nil)))
                   ((:name :array :string :dot :sub :call :regexp) nil)
                   (t t)))

               (discard-empty-blocks (body)
                 (declare (type list body))
                 (declare (inline discard-empty-blocks))
                 (delete-if (lambda (stmt)
                              (and (eq (car stmt) :block)
                                   (not (cadr stmt)))) body))

               (join (list sep1 &optional sep2)
                 (declare (inline join))
                 (with-output-to-string (out)
                   (loop :for cons :on list
                      :do (write-string (car cons) out)
                      :when (cdr cons) :do (if beautify
                                               (write-string sep1 out)
                                               (when sep2
                                                 (write-string sep2 out))))))

               (format-body (body &optional (nl *codegen-newline*))
                 (setf body (discard-empty-blocks body))
                 (if body
                     (join `("{"
                             ,@(with-indent 1 (loop :for (this next) :on body
                                                 :for line = (gencode this)
                                                 :when (and (not beautify)
                                                            (not next))
                                                   :do (setq line (ppcre:regex-replace ";+$" line ""))
                                                 :collect (indent line)))
                             ,(indent "}"))
                           nl)
                     "{}"))

               (add-spaces (&rest elements)
                 (declare (type list elements))
                 (declare (inline add-spaces))
                 (let ((elements (remove-if #'null elements)))
                   (cond
                     (beautify (format nil "~{~A~^ ~}" elements))
                     (t (apply #'concatenate 'string (loop :for (i next) :on elements
                                                        :when i :collect i
                                                        :when (and next
                                                                   (or
                                                                    (and (ppcre:scan "(?i)[a-z0-9_$]$" i)
                                                                         (ppcre:scan "^(?i)[a-z0-9_$]" next))
                                                                    (and (ppcre:scan "[+-]$" i)
                                                                         (ppcre:scan "^[+-]" next)))) :collect " "))))))

               (format-switch-body (body &optional (nl *codegen-newline*))
                 (if body
                     (let ((body (join (loop :for (c . b) :in body
                                          :if c :collect (with-indent 0.5
                                                           (indent (stick (add-spaces "case" (gencode c)) ":")))
                                          :else :collect (with-indent 0.5
                                                           (indent "default:")) :end
                                          :append (with-indent 1
                                                    (mapcar (lambda (expr) (indent (gencode expr)))
                                                            (setf body (discard-empty-blocks b)))))
                                       nl)))
                       (unless beautify
                         (setq body (ppcre:regex-replace ";+$" body "")))
                       (join (list "{" body (indent "}")) nl))
                     "{}"))

               (parenthesize (expr &rest cases)
                 (declare (inline parenthesize))
                 (let ((str (gencode expr)))
                   (loop :for i :in cases :do
                      (typecase i
                        (function (when (funcall i expr) (return (stick "(" str ")"))))
                        (t (when (eq (car expr) i) (return (stick "(" str ")")))))
                      :finally (return str))))

               (make-then (th)
                 (declare (inline make-then))
                 (let ((b th))
                   (loop :for type = (first b) :do
                      (case type
                        (:if (if (not (fourth b))
                                 ;; no else for this IF, we need the block
                                 (return (gencode `(:block (,th))))
                                 (return (gencode th))))
                        ((:while :do) (setf b (third b)))
                        ((:for :for-in) (setf b (fifth b)))
                        (t (return (gencode th)))))))

               (make-name (name)
                 (declare (inline make-name))
                 (etypecase name
                   (function (funcall name))
                   (string name)))

               (make-number (n)
                 (cond
                   ((= (floor n) n) (ppcre:regex-replace "000+$"
                                                         (format nil "~D" (floor n))
                                                         (lambda(str s e ms me rs re)
                                                           (declare (ignore str s e rs re))
                                                           (format nil "e~A" (- me ms)))))
                   (t (ppcre:regex-replace "^0\."
                                           (ppcre:regex-replace "\\.e" (format nil "~f" n) "e")
                                           "."))))

               (quote-object-prop (prop)
                 (if (or quote-keys
                         (and (stringp prop)
                              (not (is-identifier prop))))
                     (quote-string prop)
                     (if (numberp prop)
                         (make-number prop)
                         prop)))

               (gencode (ast)
                 ;; someone tell me how do I trick Emacs to indent this properly.
                 (push ast stack)
                 (prog1
                     (ast-case ast

                       (:toplevel (body) (join (mapcar #'indent
                                                       (mapcar #'gencode
                                                               (discard-empty-blocks body)))
                                               *codegen-tl-newline*))

                       (:block (body) (format-body body))

                       ((:var :const) (defs)
                        (stick (ecase (car ast)
                                 (:var "var ")
                                 (:const "const "))
                               (join (mapcar (lambda (def)
                                               (if (cdr def)
                                                   (add-spaces (make-name (car def)) "=" (gencode (cdr def)))
                                                   (make-name (car def)))) defs)
                                     ", " ",")
                               ";"))

                       (:return (expr) (stick (add-spaces "return" (gencode expr)) ";"))

                       (:throw (expr) (stick (add-spaces "throw" (gencode expr)) ";"))

                       ((:function :defun) (name args body)
                        (add-spaces (stick (add-spaces "function" (when name
                                                                    (make-name name)))
                                           "(" (join (mapcar #'make-name args) ", " ",") ")")
                                    (format-body body)))

                       (:stat (stmt)
                              (stick (gencode stmt) ";"))

                       (:array (a)
                               (if (not a) "[]"
                                   (add-spaces "[" (join (mapcar (lambda (expr)
                                                                   (parenthesize expr :seq)) a) ", " ",") "]")))

                       (:object (props)
                                (if (not props) "{}"
                                    (join `("{"
                                            ,(join (with-indent 1
                                                     (mapcar (lambda (p)
                                                               (indent
                                                                (case (cadr p)
                                                                  ((:get :set) (add-spaces
                                                                                (stick
                                                                                 (add-spaces (string-downcase (string (cadr p)))
                                                                                             (quote-object-prop (car p)))
                                                                                 "("
                                                                                 (join (mapcar #'make-name (fifth p)) ", " ",")
                                                                                 ")")
                                                                                (format-body (sixth p))))
                                                                  (t (join `(,(quote-object-prop (car p))
                                                                              ,(gencode (cdr p)))
                                                                           ": " ":"))))) props))
                                                   #.(format nil ",~%") ",")
                                            ,(indent "}")) *codegen-newline*)))

                       (:binary (op left right)
                                (let ((lvalue (gencode left))
                                      (rvalue (gencode right)))
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
                                        (let ((op (operator-string op)))
                                          (declare (type simple-string op))
                                          (write-string op out)
                                          (when (char>= (char op 0) #\A)
                                            (write-char #\Space out))
                                          (if (member (car expr) '(:num :unary-prefix))
                                              (write-string (gencode expr) out)
                                              (write-string (parenthesize expr #'needs-parens) out)))))

                       (:unary-postfix (op expr)
                                       (stick (if (member (car expr) '(:num :unary-postfix))
                                                  (gencode expr)
                                                  (parenthesize expr #'needs-parens))
                                              (operator-string op)))

                       (:num (n) (make-number n))

                       (:string (str) (quote-string str))

                       (:name (name) (make-name name))

                       (:atom (a)
                              (ecase a
                                (:true (if beautify "true" "!0"))
                                (:false (if beautify "false" "!1"))
                                (:null "null")
                                (:undefined "undefined")
                                (t (string-downcase (string a)))))

                       (:regexp (pattern modifiers)
                                (stick "/" (with-output-to-string (out)
                                             (write-regexp pattern out)) "/" modifiers))

                       (:try (tr ca fi)
                             (add-spaces "try" (gencode tr)
                                         (when ca (add-spaces "catch" (stick "(" (make-name (car ca)) ")") (gencode (cdr ca))))
                                         (when fi (add-spaces "finally" (gencode fi)))))

                       (:assign (op left right)
                                (add-spaces (gencode left)
                                            (if (eq op t)
                                                "="
                                                (stick (operator-string op) "="))
                                            (parenthesize right :seq)))

                       (:new (ctor args)
                             (with-output-to-string (out)
                               (write-string
                                (add-spaces "new" (parenthesize ctor :seq :binary :conditional :assign
                                                                (lambda (expr)
                                                                  (catch 'has-call
                                                                    (ast-walk (expr)
                                                                      (ast-case expr
                                                                        (:call () (throw 'has-call t))
                                                                        (:function () expr)))
                                                                    nil))
                                                                )) out)
                               (when args
                                 (write-string (stick "(" (join (mapcar #'gencode args) ", " ",") ")") out))))

                       (:break (label) (stick (add-spaces "break" (when label (make-name label))) ";"))

                       (:continue (label) (stick (add-spaces "continue" (when label (make-name label))) ";"))

                       (:conditional (cond then else)
                                     (add-spaces (parenthesize cond :assign :seq :conditional)
                                                 "?"
                                                 (parenthesize then :seq)
                                                 ":"
                                                 (parenthesize else :seq)))

                       (:call (expr args)
                              (stick (parenthesize expr #'needs-parens)
                                     "(" (join (mapcar (lambda (expr) (parenthesize expr :seq)) args) ", " ",") ")"))

                       (:dot (expr prop)
                             (stick (parenthesize expr #'needs-parens) "." prop))

                       (:if (cond then else)
                            (apply #'add-spaces `("if" ,(stick "(" (gencode cond) ")")
                                                       ,(if else (make-then then) (gencode then))
                                                       ,@(when else `("else" ,(gencode else))))))

                       (:for (init cond step body)
                             (let ((args (join (list (if init (ppcre:regex-replace ";+$" (gencode init) "") "")
                                                     (if cond (ppcre:regex-replace ";+$" (gencode cond) "") "")
                                                     (if step (ppcre:regex-replace ";+$" (gencode step) "") ""))
                                               "; " ";")))
                               (when (string= args "; ; ") (setf args ";;"))
                               (add-spaces "for" (stick "(" args ")")
                                           (gencode body))))

                       (:for-in (var key hash body)
                                (with-output-to-string (out)
                                  (write-string (add-spaces "for" "(") out)
                                  (write-string (add-spaces (if var
                                                                (ppcre:regex-replace ";+$" (gencode var) "")
                                                                (gencode key))
                                                            "in"
                                                            (stick (gencode hash) ")")
                                                            (gencode body)) out)))

                       (:while (cond body)
                               (add-spaces "while" (stick "(" (gencode cond) ")")
                                           (gencode body)))

                       (:with (expr body)
                              (add-spaces "with" (stick "(" (gencode expr) ")")
                                          (gencode body)))

                       (:do (cond body)
                            (add-spaces "do" (gencode body) "while" (stick "(" (gencode cond) ");")))

                       (:sub (expr sub)
                             (stick (parenthesize expr #'needs-parens) "[" (gencode sub) "]"))

                       (:seq (one two)
                             (join (list (gencode one) (gencode two)) ", " ","))

                       (:label (label body)
                               (add-spaces (stick (when label (make-name label)) ":") (gencode body)))

                       (:switch (expr body)
                                (add-spaces "switch" (stick "(" (gencode expr) ")")
                                            (format-switch-body body))))
                   (pop stack))))

        (gencode ast)))))

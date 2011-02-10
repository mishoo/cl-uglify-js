(in-package #:uglify-js)

;;; <cl-json> --- the following is taken from cl-json
(defparameter +json-lisp-escaped-chars+
  '((#\\ . #\\)
    (#\b . #\Backspace)
    (#\f . #\)
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)
    (#\u . (4 . 16)))
  "Mapping between JSON String escape sequences and Lisp chars.")
;;; </cl-json>

(defparameter *codegen-indent-level* 4)
(defparameter *codegen-indent-start* 0)
(defparameter *codegen-quote-keys* nil)
(defparameter *codegen-beautify* t)
(defparameter *codegen-tl-newline* (format nil "~%~%"))
(defparameter *codegen-newline* (format nil "~%"))

(defparameter *precedence* parse-js::*precedence*)

(let ((reserved (list
                 "abstract"
                 "boolean"
                 "break"
                 "byte"
                 "case"
                 "catch"
                 "char"
                 "class"
                 "const"
                 "continue"
                 "debugger"
                 "default"
                 "delete"
                 "do"
                 "double"
                 "else"
                 "enum"
                 "export"
                 "extends"
                 "false"
                 "final"
                 "finally"
                 "float"
                 "for"
                 "function"
                 "goto"
                 "if"
                 "implements"
                 "import"
                 "in"
                 "instanceof"
                 "int"
                 "interface"
                 "long"
                 "native"
                 "new"
                 "null"
                 "package"
                 "private"
                 "protected"
                 "public"
                 "return"
                 "short"
                 "static"
                 "super"
                 "switch"
                 "synchronized"
                 "this"
                 "throw"
                 "throws"
                 "transient"
                 "true"
                 "try"
                 "typeof"
                 "var"
                 "void"
                 "volatile"
                 "while"
                 "with"))
      (hash (make-hash-table :test #'equal)))
  (loop :for i :in reserved
     :do (setf (gethash i hash) t))
  (defun is-identifier (name)
    (declare (inline is-identifier))
    (and (not (gethash name hash))
         (ppcre:scan "^[a-zA-Z_$][a-zA-Z0-9_$]*$" name))))

(defun curry (func &rest a1)
  (lambda (&rest a2)
    (apply func (append a1 a2))))

(defstruct topval (v nil))

(defun mymap (func list)
  (iter (for i in list)
        (for v = (funcall func i))
        (finally (return (nconc one two)))
        ;; because iterate has a nasty bug: if you try to collect into
        ;; the *same* list, sometimes at start, sometimes at end,
        ;; you'll get a "nil is not a cons" error.
        (if (typep v 'topval)
            (collect (topval-v v) into one at start)
            (collect v into two))))

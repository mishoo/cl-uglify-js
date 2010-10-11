(in-package #:uglify-js)

;;; <cl-json> --- the following is taken from cl-json
(defparameter +json-lisp-escaped-chars+
  '((#\" . #\")
    (#\\ . #\\)
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
(defparameter *codegen-dot-call-no-parens* '(:name :array :string :dot :sub :call :regexp))

(defparameter *precedence* parse-js::*precedence*)

(defun operator-string (op)
  (string-downcase (string op)))

(defun dot-call-parens (expr)
  (not (member (car expr) *codegen-dot-call-no-parens*)))

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
                 "throw"
                 "throws"
                 "transient"
                 "true"
                 "try"
                 "typeof"
                 "undefined"
                 "var"
                 "void"
                 "volatile"
                 "while"
                 "with")))
  (defun is-identifier (name)
    (and (ppcre:scan "^[a-zA-Z_$][a-zA-Z0-9_$]*$" name)
         (not (member name reserved :test #'string=)))))

(defun curry (func &rest a1)
  (lambda (&rest a2)
    (apply func (append a1 a2))))

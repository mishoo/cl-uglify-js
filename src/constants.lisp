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

(defun precedence (op) (gethash op *precedence*))

(defun dot-call-no-parens (expr)
  (member (car expr) *codegen-dot-call-no-parens*))

(defun dot-call-parens (expr)
  (not (dot-call-no-parens expr)))

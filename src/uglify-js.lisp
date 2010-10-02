(in-package #:uglify-js)

;;; scope stuff

(defstruct scope
  (names (make-hash-table :test #'string=))
  (mangled (make-hash-table :test #'string=))
  (rev-mangled (make-hash-table :test #'string=))
  (cname -1)
  (refs (make-hash-table :test #'string=))
  (uses-eval nil :type boolean)
  (uses-with nil :type boolean)
  (level 0 :type integer)
  (parent nil :type scope)
  (children nil :type list))

(defun new-scope (&optional parent)
  (let ((s (make-scope :parent parent)))
    (when parent
      (push s (scope-children parent))
      (setf (scope-level s) (1+ (scope-level parent))))
    s))

(let ((digits "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$_"))
  (defun base54 (num)
    (nreverse (with-output-to-string (out)
                (loop :do
                   (write-char (char digits (mod num 54)) out)
                   (setq num (floor (/ num 54)))
                   (when (= num 0)
                     (return)))))))

(defun scope-has (scope name)
  (loop :for s = scope :then (scope-parent s) :do
     (when (gethash name (scope-names s))
       (return s))))

(defun scope-has-mangled (scope mname)
  (loop :for s = scope :then (scope-parent s) :do
     (when (gethash mname (scope-rev-mangled s))
       (return s))))

(defun scope-next-mangled (scope)
  (tagbody
   next
     (let ((m (base54 (incf (scope-cname scope)))))
       (let (prior)
         (when (and (setq prior (scope-has-mangled scope m))
                    (eq prior (gethash (gethash m (scope-rev-mangled prior))
                                       (scope-refs scope))))
           (go next))
         (when (and (setq prior (scope-has scope m))
                    (not (eq prior scope))
                    (eq (gethash m (scope-refs scope)) prior)
                    (not (scope-has-mangled prior m)))
           (go next)))
       (multiple-value-bind (val has)
           (gethash m (scope-refs scope))
         (when (and has (not val))
           (go next)))
       (when (is-identifier m)
         (go next))
       m)))

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

(defun scope-get-mangled (scope name &optional make)
  (block nil
    (when (or (scope-uses-with scope)
              (scope-uses-eval scope))
      (return name))
    (let ((s (scope-has scope name)))
      (when (not s)
        (return name))
      (multiple-value-bind (m already) (gethash name (scope-mangled s))
        (when already
          (return m))))
    (unless make
      (return name))
    (let ((m (scope-next-mangled scope)))
      (setf (gethash name (scope-mangled scope)) m
            (gethash m (scope-rev-mangled scope)) name)
      m)))

(defun scope-define (scope name)
  (when name
    (setf (gethash name (scope-names scope)) name)))

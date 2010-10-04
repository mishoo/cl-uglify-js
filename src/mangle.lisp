(in-package #:uglify-js)

(defstruct scope
  (names (make-hash-table :test #'equal))
  (mangled (make-hash-table :test #'equal))
  (rev-mangled (make-hash-table :test #'equal))
  (cname -1)
  (refs (make-hash-table :test #'equal))
  (uses-eval nil :type boolean)
  (uses-with nil :type boolean)
  (level 0 :type integer)
  (parent nil :type (or null scope))
  (children nil :type list))

(defmacro foreach-scope-parent ((scope var) &body body)
  `(loop :for ,var = ,scope :then (scope-parent ,var) :while ,var :do ,@body))

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
  (foreach-scope-parent (scope s)
    (multiple-value-bind (val has) (gethash name (scope-names s))
      (declare (ignore val))
      (when has (return s)))))

(defun scope-has-mangled (scope mname)
  (foreach-scope-parent (scope s)
    (multiple-value-bind (val has) (gethash mname (scope-rev-mangled s))
      (declare (ignore val))
      (when has (return s)))))

(defun scope-next-mangled (scope)
  (block nil
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
         (unless (is-identifier m)
           (go next))
         (return m)))))

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
          (return m)))
      (unless make
        (return name))
      (let ((m (scope-next-mangled s)))
        (setf (gethash name (scope-mangled s)) m
              (gethash m (scope-rev-mangled s)) name)
        (return m)))))

(defun scope-define (scope name)
  (when name
    (setf (gethash name (scope-names scope)) name)))

(defparameter *current-scope* nil)

(defun ast-mangle (ast &key toplevel)

  (let ((having-eval ()))

    (macrolet ((with-new-scope (&body body)
                 `(let ((*current-scope* (new-scope *current-scope*)))
                    ,@body)))

      (labels ((define (name)
                 (scope-define *current-scope* name))

               (reference (name)
                 (setf (gethash name (scope-refs *current-scope*)) t))

               (wrap-name (name &key define reference)
                 (if (is-identifier name)
                     (progn
                       (when define (define name))
                       (when reference (reference name))
                       (if (or toplevel (scope-parent *current-scope*))
                           (curry #'scope-get-mangled *current-scope* name define)
                           name))
                     name)))

        (with-new-scope
            (let ((new-ast (ast-walk (ast)
                             (ast-case expr
                               ((:function :defun) (name args body) `(,(car expr)
                                                                       ,(when name (wrap-name name :define t))
                                                                       ,@(with-new-scope
                                                                          (list (mapcar (lambda (arg)
                                                                                          (wrap-name arg :define t)) args)
                                                                                (mapcar #'walk body)))))
                               (:with ()
                                      (foreach-scope-parent (*current-scope* s)
                                        (setf (scope-uses-with s) t))
                                      nil)
                               ((:var :const) (defs) `(,(car expr)
                                                        ,(mapcar (lambda (def)
                                                                   `(,(wrap-name (car def) :define t)
                                                                      ,@(walk (cdr def))))
                                                                 defs)))
                               (:try (tr ca fi)
                                     (when ca
                                       ;; only when the catch block is present we need to deal with names
                                       `(:try ,(walk tr)
                                              ,(with-new-scope
                                                `(,(wrap-name (car ca) :define t)
                                                   ,@(walk (cdr ca))))
                                              ,(walk fi))))
                               (:name (name)
                                      (when (string= name "eval")
                                        (setf having-eval (pushnew *current-scope* having-eval)))
                                      `(:name ,(wrap-name name :reference t)))
                               (:for-in (has-var name hash body)
                                        `(:for-in ,has-var
                                                  ,(wrap-name name :define has-var :reference (not has-var))
                                                  ,(walk hash)
                                                  ,(walk body)))))))

              ;; propagate "uses-eval" to toplevel scope
              (dolist (scope having-eval)
                (unless (scope-has scope "eval")
                  (foreach-scope-parent (scope s)
                    (setf (scope-uses-eval s) t))))

              ;; now compute references
              (labels ((fixrefs (scope)
                         ;; do children first
                         (mapc #'fixrefs (scope-children scope))
                         ;; for all names that we reference, we find the
                         ;; origin scope (scope-has returns that) and
                         ;; then for all intermediary scopes we store
                         ;; the origin in refs[name].
                         (loop :for name :being :the :hash-keys :in (scope-refs scope)
                            :for origin = (scope-has scope name)
                            :do (foreach-scope-parent (scope s)
                                  (setf (gethash name (scope-refs s)) origin)
                                  (when (eq s origin) (return)))))

                       (mangle-names (scope)
                         (when (or toplevel (scope-parent scope))
                           (maphash (lambda (name v)
                                      (declare (ignore v))
                                      (scope-get-mangled scope name t))
                                    (scope-names scope)))
                         (mapc #'mangle-names (scope-children scope))))

                (fixrefs *current-scope*)
                (mangle-names *current-scope*))

              new-ast))))))

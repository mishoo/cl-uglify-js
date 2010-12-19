(in-package #:uglify-js)

(defmacro awhen++ ((var cond) &body body)
  `(let ((,var ,cond))
     (when ,var
       ,@body)))

(flet ((as-number (val)
         (handler-case
             (parse-number:parse-number val)
           (parse-error ()
             nil))))
  (defgeneric binary-op (op left right)
    (:method ((op (eql :+)) (left number) (right number))
      (+ left right))
    (:method ((op (eql :-)) (left number) (right number))
      (- left right))
    (:method ((op (eql :*)) (left number) (right number))
      (* left right))
    (:method ((op (eql :/)) (left number) (right number))
      (/ left right))
    (:method ((op (eql :<<)) (left number) (right number))
      (ash left right))
    (:method ((op (eql :>>)) (left number) (right number))
      (ash left (- right)))
    (:method ((op (eql :>>>)) (left number) (right number))
      (ash (abs left) (- right)))
    (:method ((op (eql :+)) left right)
      (format nil "~A~A" left right))
    (:method (op (left number) (right string))
      (awhen++ (right (as-number right))
        (binary-op op left right)))
    (:method (op (left string) (right number))
      (awhen++ (left (as-number left))
        (binary-op op left right)))
    (:method (op (left string) (right string))
      (awhen++ (left (as-number left))
        (awhen++ (right (as-number right))
          (binary-op op left right))))
    (:method (op left right))))

(defun best-of (ast1 ast2)
  (if (> (length (ast-gen-code ast1 :beautify nil))
         (length (ast-gen-code (if (eq (car ast2) :stat)
                                   (cadr ast2)
                                   ast2) :beautify nil)))
      ast2
      ast1))

(defun empty (b)
  (or (not b)
      (and (eq (car b) :block)
           (not (caadr b)))))

(defun rmblock (block)
  (when (and block
             (eq (car block) :block)
             (cadr block)
             (not (cadadr block)))
    (setf block (caadr block)))
  block)

(defun aborts (node)
  (when (and (eq (car node) :block)
             (cadr node))
    (setf node (car (last (cadr node)))))
  (member (car node) '(:return :throw :continue :break)))

(defun conditional (cond then &optional else)
  (if (and (eq (car cond) :unary-prefix)
           (eq (cadr cond) :!))
      (if else
          `(:conditional ,(caddr cond) ,else ,then)
          `(:binary :|\|\|| ,(caddr cond) ,then))
      (if else
          `(:conditional ,cond ,then ,else)
          `(:binary :&& ,cond ,then))))

(defun negate (c)
  (flet ((not-c ()
           `(:unary-prefix :! ,c)))
    (or (ast-case c
          (:unary-prefix (op expr)
                         (when (eq op :!)
                           expr))
          (:binary (op left right)
                   (case op
                     (:< `(:binary :>= ,left ,right))
                     (:<= `(:binary :> ,left ,right))
                     (:> `(:binary :<= ,left ,right))
                     (:>= `(:binary :< ,left ,right))
                     (:== `(:binary :!= ,left ,right))
                     (:!= `(:binary :== ,left ,right))
                     (:=== `(:binary :!== ,left ,right))
                     (:!== `(:binary :=== ,left ,right))
                     (:|\|\|| (best-of (not-c) `(:binary :&& ,(negate left) ,(negate right))))
                     (:&& (best-of (not-c) `(:binary :|\|\|| ,(negate left) ,(negate right))))))
          (:atom (what)
                 (case what
                   (:true `(:num 0))
                   (:false `(:num 1)))))
        (not-c))))

(defun ast-squeeze (ast &key
                    (sequences t)
                    (dead-code t)
                    &aux stack)
  (labels ((is-constant (node)
             (case (car node)
               ((:string :num) t)
               (t nil)))

           (in-function ()
             (loop :for i :in stack
                :for expr = (car i)
                :when (member expr '(:function :defun)) :do (return t)
                :when (member expr '(:for :for-in :while :do :switch :if)) :do (return nil)))

           (tighten (statements walk)
             (setf statements (mapcar walk statements))
             ;; remove unnecessary blocks
             (setf statements (loop :for i :in statements
                                 :if (eq (car i) :block) :nconc (cadr i)
                                 :else :collect i))

             ;; join consecutive var and const declarations
             (setf statements (loop :for this :in statements
                                 :with prev = nil
                                 :if (and prev (or (and (eq (car this) :var) (eq (car prev) :var))
                                                   (and (eq (car this) :const) (eq (car prev) :const))))
                                 :do (nconc (cadr prev) (cadr this))
                                 :else
                                 :collect this :into ret :and :do (setf prev this)
                                 :finally (return ret)))

             ;; after return. throw, break or continue, only function
             ;; and var declarations might make sense -- drop others.
             (when dead-code
               (let ((has-ended nil))
                 (setf statements (remove-if (lambda (stat)
                                               (let ((dead (and has-ended (not (member (car stat) '(:function :defun :var :const))))))
                                                 (when (not has-ended)
                                                   (setf has-ended (member (car stat) '(:return :throw :break :continue))))
                                                 (when dead
                                                   (warn "Removing unreachable code: ~A" (ast-gen-code stat)))
                                                 dead))
                                             statements))))

             ;; join consecutive statements into a :seq
             (when sequences
               (setf statements (loop :for this :in statements
                                   :with prev = nil
                                   :if (not prev)
                                   :collect this
                                   :and :do (when (eq (car this) :stat)
                                              (setf prev this))
                                   :else :if (and (eq (car prev) :stat)
                                                  (eq (car this) :stat))
                                   :do (setf (cadr prev) `(:seq ,(cadr prev)
                                                                ,(cadr this)))
                                   :else :collect this :and :do (setf prev nil))))

             ;; ;; when we're in a function and not in a loop or switch,
             ;; ;; and we encounter return, we can sometimes discard it.
             (when (in-function)
               (setf statements
                     (iter (for (stat . rest) on statements)
                           (after-each (collect stat into output))
                           (finally (return output))
                           (ast-case stat
                             (:if (co th el)
                                  (unless el
                                    ;; IF without ELSE
                                    (ast-case th
                                      (:return (ret)
                                               (unless ret
                                                 ;; RETURN undefined
                                                 (collect (funcall walk `(:if ,(negate co)
                                                                              (:block ,rest))) into output)
                                                 (finish)))
                                      (:block (stats)
                                        (let ((last (car (last stats))))
                                          (ast-case last
                                            (:return (what)
                                                     (unless what
                                                       (collect (funcall walk `(:if ,co
                                                                                    (:block ,(butlast stats))
                                                                                    (:block ,rest)))
                                                         into output)
                                                       (finish)))))))))
                             (:return (what)
                                      (when (and (not what)
                                                 (not rest)
                                                 (member (caar stack) '(:function :defun)))
                                        (finish)))))))

             statements))

    (ast-walk (ast expr walk stack)
      (ast-case expr

        (:sub (expr subscript)
              (when (eq (car subscript) :string)
                (let ((name (cadr subscript)))
                  (when (is-identifier name)
                    `(:dot ,(walk expr) ,name)))))

        (:toplevel (body) `(:toplevel ,(tighten body #'walk)))

        (:block (body) (rmblock `(:block ,(tighten body #'walk))))

        (:if (cond then else)
             ;; traverse the expressions to apply other optimizations first
             (setf cond (walk cond)
                   then (walk then)
                   else (walk else))
             ;; note that unnecesary blocks are eliminated by now.
             ;; handle possible cases where then/else are missing

             (if (empty then)
                 ;; when (unlikely) the then block is empty, negate the
                 ;; condition and use the else block instead
                 (setf cond (negate cond)
                       then else
                       else nil)
                 (if (empty else)
                     ;; when the else block is empty, make sure it's nil
                     (setf else nil)
                     ;; if we have both else and then, maybe it makes sense to switch them?
                     (let* ((a (ast-gen-code cond :beautify nil))
                            (n (negate cond))
                            (b (ast-gen-code n :beautify nil)))
                       (when (< (length b) (length a))
                         ;; the negated condition is shorter, switch
                         (let ((tmp then))
                           (setf then else
                                 else tmp
                                 cond n))))))

             ;; if we end up with both then and else blocks empty
             ;; (unlikely), return a statement with the condition
             ;; only (for possible side effects)
             (when (and (empty then) (empty else))
               (return `(:stat ,cond)))

             ;; small other optimizations possible
             (let ((ret `(:if ,cond ,then ,else)))
               (cond
                 ;; if blocks only contain one statement, we convert
                 ;; them using the conditional operator, or binary ||
                 ;; / &&
                 ((eq (car then) :stat)
                  (if else
                      (when (eq (car else) :stat)
                        (return (best-of ret `(:stat ,(conditional cond (cadr then) (cadr else))))))
                      (return (best-of ret `(:stat ,(conditional cond (cadr then)))))))
                 ;; if it's like if (cond) return foo; else return
                 ;; bar; (possibly throw too) we transform into return
                 ;; cond ? foo : bar.
                 ((and (eq (car then) (car else))
                       (member (car then) '(:return :throw)))
                  (return (best-of ret `(,(car then) ,(conditional cond (cadr then) (cadr else))))))
                 ;; last special case: if we have an "else" clause and
                 ;; we're sure that the "then" block aborts (ends with
                 ;; throw or return), we can get rid of "else".
                 ((and else (aborts then))
                  (return (best-of ret (walk `(:block ((:if ,cond ,then)
                                                       ,else))))))
                 ((and then (aborts else))
                  (return (best-of ret (walk `(:block ((:if ,(negate cond) ,else)
                                                       ,then)))))))
               ;; otherwise, we already have the result in ret
               ret))

        ((:function :defun) (name args body)
         `(,(car expr) ,name ,args ,(tighten body #'walk)))

        (:conditional (cond then else)
                      (conditional (walk cond) (walk then) (walk else)))

        ;; the try/catch/finally blocks are not really discardable
        (:try (tr ca fi)
              `(:try (:block ,(tighten (cadr tr) #'walk))
                     ,(when ca `(,(car ca) :block ,(tighten (caddr ca) #'walk)))
                     ,(when fi `(:block ,(tighten (cadr fi) #'walk)))))

        (:switch (expr body)
                 `(:switch ,(walk expr)
                           ,(loop :for (branch next) :on body
                               :for case = (walk (car branch))
                               :for body = (tighten (cdr branch) #'walk)
                               :for last = (and (not next) (last body))
                               :when (and last
                                          (eq (caar last) :break)
                                          (not (cadar last)))
                               :do (setq body (butlast body))
                               :collect `(,case ,@body))))

        (:binary (op left right)
                 (setf left (walk left)
                       right (walk right))
                 (let ((ret `(:binary ,op ,left ,right)))
                   (when (and (is-constant left)
                              (is-constant right))
                     (let ((val (binary-op op (cadr left) (cadr right))))
                       (when val
                         (setf ret (best-of ret `(,(etypecase val (string :string) (number :num)) ,val))))))
                   ret))

        (:unary-prefix (op ex)
                       (when (eq op :!)
                         (let ((ex (walk ex)))
                           (when (and (eq (car ex) :unary-prefix)
                                      (eq (cadr ex) :!))
                             (let ((p (cadr stack)))
                               (when (and (eq (car p) :unary-prefix)
                                          (eq (cadr p) :!))
                                 (return (caddr ex)))
                               (return `(:unary-prefix :! ,ex))))
                           (best-of `(:unary-prefix :! ,ex) (negate ex)))))

        (:atom (val)
               (case val
                 (:true '(:unary-prefix :! (:num 0)))
                 (:false '(:unary-prefix :! (:num 1)))))))))

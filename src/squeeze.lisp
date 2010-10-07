(in-package #:uglify-js)

(defun ast-squeeze (ast &key (no-seqs t))
  (labels ((is-constant (node)
             (case (car node)
               ((:string :num) t)
               (t nil)))

           (rmblock (block)
             (when (and block
                        (eq (car block) :block)
                        (cadr block)
                        (not (cadadr block)))
               (setf block (caadr block)))
             block)

           (best-of (ast1 ast2)
             (if (> (length (ast-gen-code ast1 :beautify nil))
                    (length (ast-gen-code (if (eq (car ast2) :stat)
                                              (cadr ast2)
                                              (car ast2)) :beautify nil)))
                 ast2
                 ast1))

           (empty (b)
             (or (not b)
                 (and (eq (car b) :block)
                      (not (caadr b)))))

           (aborts (node)
             (when (and (eq (car node) :block)
                        (cadr node))
               (setf node (car (last (cadr node)))))
             (member (car node) '(:return :throw :continue :break)))

           (conditional (cond then &optional else)
             (if (and (eq (car cond) :unary-prefix)
                      (eq (cadr cond) :!))
                 (if else
                     `(:conditional ,(caddr cond) ,else ,then)
                     `(:binary :|\|\|| ,(caddr cond) ,then))
                 (if else
                     `(:conditional ,cond ,then ,else)
                     `(:binary :&& ,cond ,then))))

           (tighten (statements)
             (let (ret)
               (setf ret (loop :for i :in statements
                            :if (eq (car i) :block) :nconc (cadr i)
                            :else :collect i))
               (setf ret (loop :for this :in ret
                            :with prev = nil
                            :if (and prev (or (and (eq (car this) :var) (eq (car prev) :var))
                                              (and (eq (car this) :const) (eq (car prev) :const))))
                            :do (nconc (cadr prev) (cadr this))
                            :else :collect this :do (setq prev this)))
               (when no-seqs
                 (return-from tighten ret))
               (error "Implement seqs"))))

    (ast-walk (ast expr walk)
      (ast-case expr

        (:sub (expr subscript)
              (when (eq (car subscript) :string)
                (let ((name (cadr subscript)))
                  (when (is-identifier name)
                    `(:dot ,(walk expr) ,name)))))

        (:toplevel (body) `(:toplevel ,(tighten (mapcar #'walk body))))

        (:block (body) (rmblock `(:block ,(tighten (mapcar #'walk body)))))

        (:if (cond then else)
             ;; traverse the expressions to apply other optimizations first
             (setf cond (walk cond)
                   then (walk then)
                   else (walk else))
             ;; note that unnecesary blocks are eliminated by now.
             ;; handle possible cases where then/else are missing
             (let ((negated (and (eq (car cond) :unary-prefix)
                                 (eq (cadr cond) :!))))
               ;; when (unlikely) the then block is empty, negate the
               ;; condition and use the else block instead
               (when (empty then)
                 (setf cond (if negated (caddr cond) `(:unary-prefix :! ,cond))
                       then else
                       else nil))
               (if (empty else)
                   ;; when the else block is empty, make sure it's nil
                   (setf else nil)
                   ;; if it's present and the condition is negated, remove the negation and swap then/else
                   (when negated
                     (setf cond (caddr cond))
                     (let ((tmp then))
                       (setf then else
                             else tmp))))
               ;; if we end up with both then and else blocks empty
               ;; (unlikely), return a statement with the condition
               ;; only (for possible side effects)
               (when (and (empty then) (empty else))
                 (return `(:stat ,cond))))

             ;;
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
                  ;; XXX: isn't it better if we walk() this?
                  (return `(:block ((:if ,cond ,then)
                                    ,else))))
                 ;; otherwise, we already have the result in ret
                 (t ret))))

        ((:function :defun) (name args body)
         `(,(car expr) ,name ,args ,(tighten (mapcar #'walk body))))

        (:conditional (cond then else)
                      (conditional (walk cond) (walk then) (walk else)))

        ;; we need the following because try/catch/finally blocks are not really discardable
        (:try (tr ca fi)
              `(:try (:block ,(tighten (mapcar #'walk (cadr tr))))
                     ,(when ca `(,(car ca) :block ,(tighten (mapcar #'walk (caddr ca)))))
                     ,(when fi `(:block ,(tighten (mapcar #'walk (cadr fi)))))))

        (:switch () (warn "TODO: switch -- walk the tree"))

        (:binary () (warn "TODO: binary -- resolve simple expressions"))))

    ))

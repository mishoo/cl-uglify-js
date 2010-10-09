(cl:defpackage #:cl-uglify-js.main
  (:use #:cl #:cl-uglify-js #:unix-options))

(in-package #:cl-uglify-js.main)

(defun uglify-stream (stream &key (beautify t) no-sequences keep-dead-code mangle-toplevel)
  (ast-gen-code (ast-mangle (ast-squeeze (parse-js:parse-js stream)
                                         :sequences (not no-sequences)
                                         :dead-code (not keep-dead-code))
                            :toplevel mangle-toplevel)
                :beautify beautify))

(defun main (argv)
  (declare (ignore argv))
  (with-cli-options () (beautify no-sequences no-comment keep-dead-code mangle-toplevel overwrite &free rest)
    (let ((input-file (car rest))
          (output-file (cadr rest)))
      (macrolet ((with-input-file ((input) &body body)
                   (let ((thunk (gensym)))
                     `(let (,input)
                        (flet ((,thunk () ,@body))
                          (if input-file
                              (with-open-file (in input-file)
                                (setq ,input in)
                                (,thunk))
                              (progn
                                (setq ,input *standard-input*)
                                (,thunk))))))))
        (let ((result (with-input-file (input)
                        (uglify-stream input :beautify beautify
                                       :no-sequences no-sequences
                                       :keep-dead-code keep-dead-code
                                       :mangle-toplevel mangle-toplevel))))

          ;; parse-js's lexer doesn't yet have an option to return comments
          ;;
          ;; (unless no-comment
          ;;   (let ((comments (with-input-file (input)
          ;;                     (loop :with lexer = (parse-js:lex-js input)
          ;;                        :for token = (lexer)
          ;;                        :while (eq (car token) :comment) :collect (cadr token)))))))

          (if (or overwrite output-file)
              (with-open-file (out (if overwrite input-file output-file)
                                   :direction :output
                                   :if-exists :overwrite
                                   :if-does-not-exist :create)
                (write-string result out))
              (write-string result *standard-output*)))))))

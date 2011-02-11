(in-package #:uglify-js)

(defun split-code (code &optional (maxlen (* 32 1024)))
  "Inserts newlines into the given `code' (must be a string) as soon
as lines are bigger than `maxlen' (default 32K).  Note that the lines
in the output will generally be bigger than `maxlen'.  This function
splits code only *before* a :KEYWORD, :ATOM, :NAME or :PUNC, and it
never splits *after* a :KEYWORD (in order not to break the JS
semantics, which say that no newline is allowed between return or
throw and their arguments)."
  (with-input-from-string (stream code)
    (let ((this-token nil)
          (prev-token nil)
          (splits (list 0))
          (next-token (lex-js stream :include-comments t)))
      (labels ((current-length ()
                 (- (token-pos this-token) (car splits)))
               (split-here ()
                 (push (token-pos this-token) splits)))
        (parse-js (lambda (&rest args)
                    (setf this-token (apply next-token args))
                    (unless (and prev-token
                                 (eq :keyword (token-type prev-token)))
                      (when (> (current-length) maxlen)
                        (case (token-type this-token)
                          ((:keyword :atom :name :punc) (split-here)))))
                    (setf prev-token this-token)) :ecma-version 5)
        (format nil "~{~A~^~%~}"
                (loop :for (from to) :on (nreverse splits)
                   :collecting (subseq code from to)))))))

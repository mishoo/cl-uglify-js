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
    (let ((tokenizer (lex-js stream :include-comments t))
          (this-token nil)
          (prev-token nil)
          (splits (list 0)))
      (labels ((next-token ()
                 (setf prev-token this-token
                       this-token (funcall tokenizer)))
               (current-length ()
                 (- (token-pos this-token) (car splits)))
               (split-here ()
                 (push (token-pos this-token) splits)))
        (iter (for i = (next-token))
              (until (eq :eof (token-type i)))
              (when (and prev-token
                         (eq :keyword (token-type prev-token)))
                (next-iteration))
              (when (> (current-length) maxlen)
                (when (member (token-type this-token) '(:keyword :atom :name :punc))
                  (split-here))))
        (format nil "~{~A~^~%~}"
                (iter (for (from to) on (nreverse splits))
                      (collect (subseq code from to))))))))

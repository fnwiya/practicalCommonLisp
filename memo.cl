;; Chapter 2
(defun hello-world () (format t "hello, world"))

;; Chapter 3
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

;; (defun dump-db ()
;;   (dolist (cd *db*)
;;     (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db ()
  (format t  "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lamda (cd) (equal (getf cd :artist) artist)))

;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (cd)
;;       (and
;;        (if title    (equal (getf cd :title)  title)  t)
;;        (if artist   (equal (getf cd :artist) artist) t)
;;        (if rating   (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title)  title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defmacro backwards (expr) (reverse expr))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

;; Chapter 5
(defun verbose-sum (x y)
  "Sum two numbers"
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun foo-sup (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

;; Chapter 7
;; (defmacro when (condition &rest body)
;;   `(if ,condition (progn ,@body)))

(defun fibo (max)
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= max n) cur)))

;; Chapter 8
(defun primep (numbers)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; (defmacro do-primes (var-and-range &rest body)
;;   (let ((var (first var-and-range))
;;         (start (second var-and-range))
;;         (end (third var-and-range)))
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;;          ((> ,var ,end))
;;        ,@body)))

;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;;        ((> ,var ,end))
;;      ,@body))

(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


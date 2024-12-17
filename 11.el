(require 'dash)

(defun rule (stone)
  (-let* ((s-stone (number-to-string stone))
          (digits (length s-stone)))
    (cond
     ((equal stone 0) '(1))
     ((zerop (% digits 2))
      (->> (list (substring s-stone 0 (/ digits 2))
                 (substring s-stone (/ digits 2)))
           (-map #'string-to-number)))
     (t (list (* 2024 stone))))))

(defun single-round (stones)
  (-mapcat #'rule stones))

(defun part1 (stones)
  (->>
   stones
   (funcall (-iteratefn #'single-round 25))
   (length)))

(defun single-round-dedup (counts-ht)
  (-let [new-counts-ht (make-hash-table)]
    (maphash
     (lambda (stone count)
       (dolist (new-stone (rule stone))
         (--> (gethash new-stone new-counts-ht 0)
              (+ count it)
              (puthash new-stone it new-counts-ht))))
     counts-ht)
    new-counts-ht))

(defun part2 (stones)
  (let ((initial-ht (make-hash-table))
        (total 0))
    (dolist (stone stones)
      (puthash stone 1 initial-ht))
    (->> initial-ht
        (funcall (-iteratefn #'single-round-dedup 75))
        (maphash (lambda (_ count) (cl-incf total count))))
    total))

(setq example '(125 17))
;; (setq input '(redacted))
(part1 input)
(part2 input)

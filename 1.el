(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (--> input-string (split-string it "\n" t)
       (-map (lambda (line) (split-string line)) it)
       (--map (-map #'string-to-number it) it)))

(setq example '((3 4)
               (4 3)
               (2 5)
               (1 3)
               (3 9)
               (3 3)))

(defun part1 (pairs)
  (-let [(a b) (-unzip pairs)]
    (sort a #'<)
    (sort b #'<)
    (->> (cl-mapcar (lambda (left right) (abs (- left right))) a b)
         (cl-reduce #'+))))

(defun part2 (pairs)
  (-let* (((a b) (-unzip pairs))
         (frequencies (-annotate (lambda (a) (--count (equal it a) b)) a)))
    (-sum (--map (-let [(a . b) it] (* a b)) frequencies))))

(setq input (parse-input (read-input "1.txt")))
(part1 input)
(part2 input)

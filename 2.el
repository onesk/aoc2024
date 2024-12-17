(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (--> input-string (split-string it "\n" t)
       (-map (lambda (line) (split-string line)) it)
       (-tree-map #'string-to-number it)))

(defun safep (report)
  (let* ((pairs (-zip-lists report (cdr report)))
         (diffs (-map (-applify #'-) pairs))
         (increasing (--all? (<= it 0) diffs))
         (decreasing (--all? (>= it 0) diffs))
         (within-limits (--all? (->> it abs ((lambda (n) (and (>= n 1) (<= n 3))))) diffs)))
    ;; (list increasing decreasing within-limits)))
    (and (or increasing decreasing) within-limits)))

(defun relaxed-safep (report)
  (let* ((n (length report))
         (indices (-iota n))
         (punctured (--map (-remove-at it report) indices)))
    (-any? #'safep punctured)))

(setq example '((7 6 4 2 1)
                (1 2 7 8 9)
                (9 7 6 2 1)
                (1 3 2 4 5)
                (8 6 4 4 1)
                (1 3 6 7 9)))

(defun part1 (input)
  (-count #'safep input))

(defun part2 (input)
  (-count #'relaxed-safep input))

(setq input (parse-input (read-input "2.txt")))
(part1 input)
(part2 input)

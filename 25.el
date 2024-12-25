(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-item (item-string)
  (let* ((matrix (--> item-string
                     (split-string it "\n" t)
                     (--map (seq-into it 'vector) it)
                     (seq-into it 'vector)))
         (height (length matrix))
         (width  (if (zerop height) 0 (length (aref matrix 0))))
         runs shape min-h)
    (dotimes (i width)
      (push (->> (--map (aref (aref matrix it) i) (-iota height))
                 ((lambda (column) (-let [head (car column)]
                                     (list head
                                           (length (--filter (= it head) column))
                                           (length (--filter (not (= it head)) column)))))))
            runs))
    (setq runs (nreverse runs))
    (setq shape (cond
      ((--all? (= ?# (car it)) runs) (cons :lock (-map #'cadr runs)))
      ((--all? (= ?. (car it)) runs) (cons :key  (-map #'caddr runs)))))
    (dotimes (i width)
      (cl-decf (nth (1+ i) shape)))
    shape))

(defun parse-input (input-string)
  (--> input-string
       (split-string it "\n\n" t)
       (-map #'parse-item it)))

(defun part1 (input)
  (-let [ans 0]
    (dolist (lock (--filter (eq :lock (car it)) input))
      (dolist (key (--filter (eq :key (car it)) input))
        (-let [heights (-zip-with #'+ (cdr lock) (cdr key))]
          (when (>= 5 (-max heights))
            (cl-incf ans)))))
    ans))

(setq example (parse-input "
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"))

(setq input (parse-input (read-input "25.txt")))
(part1 input)

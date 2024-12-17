(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((lines (--> input-string (split-string it"\n" t) (-map #'string-trim it)))
          (rows (length lines))
          (cols (length (nth 0 lines))))
    (list rows cols (--> lines
                         (-map (-rpartial #'seq-into 'vector) it)
                         (seq-into it 'vector)))))

(defun read-grid (grid x y)
  (-let [(rows cols matrix) grid]
    (and (>= x 0) (>= y 0) (< x rows) (< y cols)
         (-> matrix
             (aref x)
             (aref y)))))

(defun dfs (id color x y)
  (when (equal color (read-grid grid x y))
    (unless (-> marked (aref x) (aref y))
      (setf (aref (aref marked x) y) id)
      (dolist (dir '((1 0) (0 1) (-1 0) (0 -1)))
        (-let [(dx dy) dir] (dfs id color (+ x dx) (+ y dy)))))))

(defun mark-regions (grid)
  (-let* (((rows cols _) grid)
          (marked (-> (-map (lambda (_) (make-vector rows nil)) (-iota cols)) (seq-into 'vector)))
          (max-id 0))
    (dolist (start (-table-flat #'cons (-iota rows) (-iota cols)))
      (-let [(sx . sy) start]
        (unless (read-marked sx sy)
          (dfs max-id (read-grid grid sx sy) sx sy)
          (cl-incf max-id))))

    (cons max-id marked)))

(defun read-marked (x y)
  (and (>= x 0) (>= y 0) (< x rows) (< y cols) (-> marked (aref x) (aref y))))

(defun add-area ()
  (seq-doseq (row marked)
    (seq-doseq (id row)
      (cl-incf (aref area id)))))

(defun add-perimeter (traversal-order)
  (let (cur prev)
    (dolist (coord traversal-order)
      (-let [(x . y) coord]
        (setq cur (read-marked x y))
        (unless (equal cur prev)
          (when cur (cl-incf (aref perimeter cur)))
          (when prev (cl-incf (aref perimeter prev))))
        (setq prev cur)))))

(defun compute-stats (grid postprocess)
  (-let* (((max-id . marked) (mark-regions grid))
          ((rows cols _) grid)
          (area (make-vector max-id 0))
          (perimeter (make-vector max-id 0)))
    (add-area)
    (add-perimeter (-table-flat #'cons
                                (-iota (+ 2 rows) -1) (-iota cols)))
    (add-perimeter (-table-flat (-flip #'cons)
                                (-iota (+ 2 cols) -1) (-iota rows)))
    (funcall postprocess)
    (-> (-zip-with #'*
                   (seq-into area 'list)
                   (seq-into perimeter 'list))
        (-sum))))

(defun hflip (l)
  (-let [(a b c d) l] (list c d a b)))

(defun mirror (l)
  (-let [(a b c d) l] (list a c b d)))

(defun transforms (l)
  (-map (-rpartial #'funcall l) (-map (-partial #'apply #'-compose) (-powerset '(hflip mirror)))))

(defun straighten-one (l)
  (-let [(a b c d) l]
    (when (and (equal a b) (not (equal a c)) (not (equal a d)))
      (when a (cl-decf (aref perimeter a))))))

(defun straighten-2x2 (x y)
  (-> (-table-flat #'read-marked (list x (1- x)) (list y (1- y)))
       (transforms)
       (-each #'straighten-one)))

(defun perimeter-to-sides ()
  (dolist (coord (-table-flat #'cons (-iota (1+ rows)) (-iota (1+ cols))))
    (-let [(x . y) coord]
      (straighten-2x2 x y))))

(defun part1 (grid)
  (compute-stats grid (lambda ())))

(defun part2 (grid)
  (compute-stats grid #'perimeter-to-sides))

(setq first-example
      (parse-input
       (mapconcat #'identity '("AAAA"
                               "BBCD"
                               "BBCC"
                               "EEEC") "\n")))

(setq example
      (parse-input
       (mapconcat #'identity '("RRRRIICCFF"
                               "RRRRIICCCF"
                               "VVRRRCCFFF"
                               "VVRCCCJFFF"
                               "VVVVCJJCFE"
                               "VVIVCCJJEE"
                               "VVIIICJJEE"
                               "MIIIIIJJEE"
                               "MIIISIJEEE"
                               "MMMISSJEEE") "\n")))

(setq max-lisp-eval-depth 40000)
(setq input (parse-input (read-input "12.txt")))
(part1 input)
(part2 input)

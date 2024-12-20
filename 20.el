(require 'dash)
(require 'ht)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((trimmed (-> input-string (string-trim)
                       (split-string "\n" t)
                       (seq-into 'vector)))
          (height (length trimmed))
          (width (length (aref trimmed 0)))
          (map (-> (-map (lambda (_) (make-vector width nil)) (-iota height))
                   (seq-into 'vector)))
          start end)

    (dotimes (i height)
      (dotimes (j width)
        (setf (aref (aref map i) j)
              (pcase (aref (aref trimmed i) j)
                (?# 'W)
                (?. '_)
                (?S (setq start (cons i j))
                    '_)
                (?E (setq end (cons i j))
                    '_)))))

    (list start end height width map)))

(defun make-queue () (cons nil nil))

(defun push-queue (elt queue)
  (cond
   ((and (not (car queue)) (not (car queue)))
    (setf (car queue) (list elt))
    (setf (cdr queue) (car queue)))
   (t (setf (cdr (cdr queue)) (list elt))
      (setf (cdr queue) (cdr (cdr queue))))))

(defun pop-queue (queue)
  (let ((head (caar queue)))
    (if (eq (car queue) (cdr queue))
        (progn
          (setf (car queue) nil)
          (setf (cdr queue) nil))
      (setf (car queue) (cdar queue)))
    head))

(defun read-grid (grid x y)
  (-let [(rows cols matrix) grid]
    (and (>= x 0) (>= y 0) (< x rows) (< y cols)
         (-> matrix
             (aref x)
             (aref y)))))

(setq all-dirs '((1 0) (0 -1) (-1 0) (0 1)))

(defun bfs (grid start)
  (let ((queue (make-queue))
        (paths (ht)))

    (push-queue start queue)
    (ht-set! paths start 0)

    (while (car queue)
      (-let* ((cc (pop-queue queue))
              ((cx . cy) cc)
              (cur-path (ht-get paths cc)))

        (dolist (dir all-dirs)
          (-let* (((dx dy) dir)
                  (nx (+ cx dx))
                  (ny (+ cy dy))
                  (map-sym (read-grid grid nx ny))
                  (nc (cons nx ny)))

            (when (and (not (ht-get paths nc)) (eq map-sym '_))
              (ht-set! paths nc (1+ cur-path))
              (push-queue nc queue))))))
    paths))

(defun cheats (input max-skips)
  (-let* (((start finish . grid) input)
          ((height width) grid)
          (forward-paths (bfs grid start))
          (backward-paths (bfs grid finish))
          (shortest-path (ht-get forward-paths finish))
          (all-skips (let* ((min-d (- max-skips))
                           (cnt (1+ (* 2 max-skips)))
                           (range (-iota cnt min-d)))
                       (->> (-table-flat #'list range range)
                            (-filter (-lambda ((x  y)) (>= max-skips (+ (abs x) (abs y))))))))
          (ans 0) savings)
    (dolist (coord (-table-flat #'cons (-iota height) (-iota width)))
      (-let [(x . y) coord]
        (dolist (dir all-skips)
          (-let* (((dx dy) dir)
                  ((nx . ny) (cons (+ x dx) (+ y dy)))
                  (p1 (ht-get forward-paths (cons x y)))
                  (p2 (ht-get backward-paths (cons nx ny))))
            (when (and p1 p2)
              (-let [alt-path (+ (abs dx) (abs dy) p1 p2)]
                (when (< alt-path shortest-path)
                  (push (- shortest-path alt-path) savings))))))))
    savings))

(defun part1 (input)
  (->> (cheats input 2) (--filter (>= it 100)) (length)))

(defun part2 (input)
  (->> (cheats input 20) (--filter (>= it 100)) (length)))

(setq example (parse-input "
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"))

(setq input (parse-input (read-input "20.txt")))
(part1 input)
(part2 input)

(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((lines (split-string input-string "\n" t))
          (rows (length lines))
          (cols (length (nth 0 lines)))
          (antennae-hash (make-hash-table))
          antennae)
    (dotimes (i rows)
      (dotimes (j cols)
        (-let [type (aref (nth i lines) j)]
          (unless (eql type ?.)
            (--> (gethash type antennae-hash)
                 (puthash type (cons (cons i j) it) antennae-hash))))))

    (maphash (lambda (type coords) (push (cons type coords) antennae)) antennae-hash)
    (list rows cols antennae)))

(setq example
      (parse-input
       (mapconcat #'identity '("............"
                               "........0..."
                               ".....0......"
                               ".......0...."
                               "....0......."
                               "......A....."
                               "............"
                               "............"
                               "........A..."
                               ".........A.."
                               "............"
                               "............") "\n")))

(defun inside (rows cols x y)
  (and (>= x 0) (>= y 0) (< x rows) (< y cols)))

(defun antinode-locations (loc1 loc2)
  (-let* ((((x1 . y1) (x2 . y2)) (list loc1 loc2))
          (dx (- x2 x1))
          (dy (- y2 y1)))
    (unless (and (zerop dx) (zerop dy))
      (list
       (cons (+ x2 dx) (+ y2 dy))
       (cons (- x1 dx) (- y1 dy))))))

(defun harmonic-antinode-locations (rows cols loc1 loc2)
  (-let* ((((x1 . y1) (x2 . y2)) (list loc1 loc2))
          (dx (- x2 x1))
          (dy (- y2 y1))
          (mx-delta (max (abs dx) (abs dy)))
          (mx-dim (max rows cols))
          (mx-steps (/ (+ mx-dim (1- mx-delta)) (if (zerop mx-delta) 1 mx-delta))))
    (unless (and (zerop dx) (zerop dy))
      (->> (-iota (1+ (* 2 mx-steps)) (- mx-steps))
           (-map (lambda (f) (cons (+ x1 (* f dx)) (+ y1 (* f dy)))))
           (-filter (-lambda ((x . y)) (inside rows cols x y)))))))

(defun antinodes-per-antenna (coords)
  (-table-flat #'antinode-locations coords coords))

(defun harmonic-antinodes-per-antenna (rows cols coords)
  (-table-flat (-partial #'harmonic-antinode-locations rows cols) coords coords))

(defun part1 (stmt)
  (-let* (((rows cols antennae) stmt)
          (all-antinodes (-map (-compose #'antinodes-per-antenna #'cdr) antennae)))
    (->> all-antinodes
         (-flatten)
         (-distinct)
         (-filter (-lambda ((x . y)) (inside rows cols x y)))
         (length))))

(defun part2 (stmt)
  (-let* (((rows cols antennae) stmt)
          (all-antinodes (-map
                          (-compose (-partial #'harmonic-antinodes-per-antenna rows cols) #'cdr) antennae)))
    (->> all-antinodes
         (-flatten)
         (-distinct)
         (length))))

(setq input (parse-input (read-input "8.txt")))
(part1 input)
(part2 input)
